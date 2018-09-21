FE_model <- function(st_date, dframe, window_length ) {
  # Run linear regression on every window and extract the coefficients
  # for the time values only
  # Arguments:
  #     st_date - a date indicating the start day of the window
  #     dframe - a data frame of prices
  #     window_length - the length of the window
  # Returns:
  #     fe_coefs - the coefficients from the fe TPD model. 1 coefficient for
  #         each time unit in window_length EXCEPT the first
  #         Length is window length - 1

  # Get the dates of each day in this window
  win_dates <- get_win_dates(st_date, window_length)

  # Subset dframe by the dates in this window
  # dframe_win <- dframe %>%
  #   filter(times_index %in% win_dates)


  dframe_win <- dframe[dframe$times_index %in%
                             win_dates, ]



  # Run the linear model and extract the coefficients from the regression
  lmfun_list <- lmfun(dframe_win)

  fe_coefs <- lmfun_list$fe_coefs
  diagnostics <- lmfun_list$diagnostics

  # fe_coefs should equal the length of window minus the 1. The reason it is -1
  # is due to the fact that 1 coefficient is sacrificed to the intercept
  if ((length(fe_coefs) + 1) != window_length){
    # TODO Should this be an error?
    stop(paste("The number of time coefficients does not match the window",
               "length? Check that your times are correct\n"))
  }

  list(data.frame(fe_coefs),
       diagnostics)
}

get_diagnostics <- function (dframe){

  # contrib_rids_pc - the % of ids which exist in the window that contribute
  # contrib_rids_nm - the number of ids which exist in the window that contribute
  # total_rids_in_data - number of rids in the entire dataset
  # total_rids_in_window - number of rids which exist in the window
  # num_records_in_window - The quantity of data in this window
  # QU_1st - 25% of the id's have less than this many entries in the window
  # gm_mean_entries_per_rid - the geometric mean number of entries per rid
  # mean_entries_per_rid - the mean number of entries per rid
  # median_entries_per_rid - the median number of entries per rid
  # QU_3rd - 25% of the id's have less than this many entries in the window

  entries_per_rid <- dframe %>%
    droplevels() %>%
    group_by(id) %>%
    summarise(n = n()) %>%
    pull(n)

  data.frame(contrib_rids_pc = mean(entries_per_rid > 1) * 100,
             contrib_rids_nm = sum(entries_per_rid > 1),
             total_rids_in_data = nlevels(dframe$id),
             total_rids_in_window = length(entries_per_rid),
             num_records_in_window = nrow(dframe),
             QU_1st = quantile(entries_per_rid, probs = .25),
             gm_mean_entries_per_rid = gm_mean(entries_per_rid),
             mean_entries_per_rid = mean(entries_per_rid),
             median_entries_per_rid = median(entries_per_rid),
             QU_3rd = quantile(entries_per_rid, probs = .75))

}

#' @import MatrixModels
lmfun <- function(dframe){
  # Regress the dates and id's as factors against logprice
  # Arguments
  #   dframe - a dframe frame with logprices, weights and id's
  # Returns
  #   modelOutput - the output of the linear model


  diagnostics <- get_diagnostics(dframe)

  # Refactor the dates here. Otherwise columns are created in the regression
  # matrix with all zeros, corresponding to dates not in the current window
  dframe$timefact <- factor(dframe$times_index)
  dframe$IDfact <- factor(dframe$id)

  # glm uses the alphabetically first id as the reference. However, if this
  # value doesn't appear in the then all other values are being compared
  # to a number that is essentially zero. Hence you can get crazily high numbers
  # like indexes of 10^50 from normal looking data
  dframe <- within(dframe,
                   IDfact <- relevel(IDfact,
                                     ref = as.character(dframe$IDfact[1])))


  # Regression doesn't work if there is only 1 item in the time window.
  if (nlevels(dframe$IDfact) == 1) {
    glm_formula <- dframe$logprice ~ dframe$timefact
  } else {
    glm_formula <- dframe$logprice ~ dframe$timefact + dframe$IDfact
  }


  # make the design matrix - model.Matrix is used due to support for
  # sparse matrices
  design_mat <- model.Matrix(glm_formula, sparse = TRUE)

  # Run the regression
  all_coefs <-  coef(glm4(glm_formula,
                          weights = dframe$weight,
                          sparse = TRUE))

  # There are coefficients returned for each time period, and each product.
  # we are only interested in change of price wrt time - so only keep theses
  # coefficients. Theses rownames start with timefact
  rows_keep <- grepl(".*timefact.*", names(all_coefs))

  # subest to just timefact values, and return in a list
  list(fe_coefs = all_coefs[rows_keep],
       diagnostics = diagnostics)
}


get_fe_list <- function (fe_indexes,
                         dframe,
                         window_st_days,
                         window_length) {
  # Takes the lm model outputs and makes a list with of dataframes containing
  # Regression coefficients (i.e. indexes) and correct dates
  #
  # Arguments:
  #     FEindxes - is a matrix of dim(window_length, num_windows) containing all
  #         of the output coefficients from the lm. all values converted back
  #         from log and 1 append as the first month in each case
  #     window_st_days - a sequence of the start date of each window
  # Returns:
  #     fe_list - a list of dataframes. Length is number of windows.
  #         nrow of each data frame is window length.
  #         The data frames contain:
  #             FE indexes for each window
  #             the window id (numeric)
  #             the dates

  fe_list <- list()

  # Loop over each window and construct the DF
  for (i in 1:ncol(fe_indexes)){
    # get the times_indexes for the current window
    times_temp <- get_win_dates(window_st_days[i],
                                window_length = window_length)
    # Convert theses to times, and keep only unique values
    times_temp <- dframe$times[dframe$times_index %in% times_temp]
    times_temp <- unique(times_temp)

    # Make the df for this list entry
    fe_list[[i]] <- data.frame(
      price_date = times_temp,
      fe_indexes = fe_indexes[, i],
      window_id = i)

    # row names are junk due to: times as factors, and rbinding rows
    rownames(fe_list[[i]]) <- c()
  }

  fe_list
}


get_fews_df <- function (fe_list, window_length, splice_pos) {
  # Splice the windows together to produce a continuous time series of indexes
  # Arguments:
  #   fe_list - output from get_fe_list. See that  function for details
  #   window_length -  window_length - the number of time_units per window
  #   splice_pos - the index on which to splice the windows. Can also be
  #       'mean' which is the geometric mean of all possible values of splice_pos


  # Initialise the df with a 1 for the first time period
  last_date <- tail(fe_list[[1]]$price_date, 1)

  fews <- data.frame(price_date = last_date,
                     fe_indexes = 1,
                     window_id = 1)

  # Loop over the windows, starting at the second window.
  # Cannot splice on the first
  for (i in 2: length(fe_list)){
    old_window <- fe_list[[i - 1]]$fe_indexes
    new_window <- fe_list[[i]]$fe_indexes

    # Get the previous FEWS index value
    old_fews <- fews$fe_indexes[nrow(fews)]

    update_factor <- splice_update (old_window,
                                    new_window,
                                    splice_pos = splice_pos)

    # Get the "new" FEWS index value
    new_fews <- old_fews * update_factor

    # buid up the new row
    # price_date is the last date in the current window
    last_date <- tail(fe_list[[i]]$price_date, 1)

    new_row <- data.frame(price_date = last_date,
                          fe_indexes = new_fews,
                          window_id = i)

    fews <- rbind(fews, new_row)
  }

  return (fews)
}


splice_update <- function (win_old, win_new, splice_pos){
  # Calculate the update factor for splicing two windows
  # Arguments
  #   win_old - the indexes for the previous window
  #   win_new - the indexes for the current window
  #   splice_pos - an integer for the time period on which to splice
  #     the windows. Can also be 'mean' or 'window' for different splice types
  #
  # Returns
  #   update_factor -  a single number which is the splice update factor

  stopifnot(length(win_old) == length(win_new))
  w <- length(win_old)
  # As the old window starts 1 entery earlier in time than the new window,
  # adding a NaN to the start of the new window makes the indexes of the 2
  # windows align. This value is never used in the calculations
  win_new <- c(NaN, win_new)

  # Variable names chosen to follow notation in IndexNumR Package
  # https://cran.r-project.org/web/packages/IndexNumR/vignettes/indexnumr.html
  Pw_new <- win_new[w]
  Pw1_new <- win_new[w + 1]
  Pw_old  <- win_old[w]

  if (splice_pos == "mean") {
    t_accum = c()
    for (t in seq(from = 2, to = w - 1, by = 1)) {
      Pt1_new <- win_new[t + 1]
      Pt1_old <- win_old[t + 1]
      t_accum <- c(t_accum, ((Pw1_new / Pt1_new) / (Pw_old / Pt1_old)))
    }
    update_factor <- gm_mean(t_accum)

  }else if (splice_pos == "movement") {
    update_factor <- (Pw1_new/Pw_new)

  } else if(splice_pos == "alan"){
    t_accum <- c() # Accumulator for the t loop
    for (t in seq(from = 2, to = w-1, by = 1)) {
      Pt_new <- win_new[t]
      Pt_old <- win_old[t]

      t_accum <- c(t_accum, ((Pw_new / Pt_new) / (Pw_old / Pt_old)))
    }
    update_factor <- (Pw1_new / Pw_new) * gm_mean(t_accum)
  }

  else {
    Pn_new <- win_new[splice_pos]
    Pn_old <- win_old[splice_pos]
    update_factor <- (Pw1_new / Pn_new) / (Pw_old / Pn_old )
  }


  return (update_factor)
}




