check_inputs <- function (times = times, logprice = logprice, id = id,
                          weight = weight, window_length = window_length,
                          splice_pos = splice_pos) {
  # Function to confirm that all inputs are correct shape and class
  # Returns all inputs, but some may be modified to the correct data type

  if (missing(weight) | is.null(weight)){
    weight <- rep(1, length(times))
    cat("\nNo weighting assigned...All weights set to 1\n")
  }


  if (anyNA(times) | anyNA(logprice) | anyNA(id) | anyNA(weight)){
    stop("Data contains NA values")
  }

  if (any(c(is.infinite(times),
            is.infinite(logprice),
            is.infinite(weight)))){
    stop("Data contains Infinite values")
  }


  if (length(times) != length(logprice)){
    stop("times and logprice should be vectors of the same length")
  }else if (length(times) != length(id)){
    stop("times and id should be vectors of the same length")
  }else if (length(times) != length(weight)){
    stop("times and weight should be vectors of the same length")
  }else if (!(length(window_length) == 1 &
              class(window_length) %in% c("numeric", "integer"))) {
    stop("window_length should be a single number")
  }

  # Times must be in a type which can be ordered - otherwise the windows
  # are meaningless
  if (!(class(times) %in% c("Date", "numeric", "integer"))){
    stop("times must be either a Date or numeric type")
  }


  splice_pos_all <- c("window", "half", "movement", "mean", "alan")
  if (!( is.numeric(splice_pos) | is.integer(splice_pos))) {
    # splice_pos not a number
    # convert to lower case, to help user error
    splice_pos <- tolower(splice_pos)
    if (!(splice_pos %in% splice_pos_all)) {
      stop("splice_pos of ", splice_pos,
           " is not a valid option. \n You must input a number or one of: ",
           paste(splice_pos_all, collapse = ", "))
    }
    # Theses two options all easy to convert to a number:
    if (splice_pos ==  "window")  splice_pos <- 2
    if (splice_pos == "half") splice_pos <- ceiling(window_length / 2)

  } else{
    # splice_pos is a numeric or integer
    if (splice_pos %% 1 != 0){
      stop ("Splice position must be a whole number")
    } else if (splice_pos > window_length){
      stop ("Splice position must be less than window length")
    }
  }

  return (list(times = times, logprice = logprice, id = id, weight = weight,
               window_length = window_length, splice_pos = splice_pos))
}




"%=%" <- function(lhs, rhs) {
  # Special equals to assign multiple entries at once - like python tuples
  # stolen from here:
  # https://stackoverflow.com/questions/1826519/how-to-assign-from-a-function-which-returns-more-than-one-value
  # %=% is used opposed to := because := is used by data.table package
  # Example usage:
  # c(a, b) %=% functionReturningAListWithTwoValues()

  frame <- parent.frame()
  lhs <- as.list(substitute(lhs))
  if (length(lhs) > 1)
    lhs <- lhs[-1]
  if (length(lhs) == 1) {
    do.call(`=`, list(lhs[[1]], rhs), envir = frame)
    return(invisible(NULL))
  }
  if (is.function(rhs) || is(rhs, "formula"))
    rhs <- list(rhs)
  if (length(lhs) > length(rhs))
    rhs <- c(rhs, rep(list(NULL), length(lhs) - length(rhs)))
  for (i in 1:length(lhs))
    do.call(`=`, list(lhs[[i]], rhs[[i]]), envir = frame)
  return(invisible(NULL))
}







get_window_st_days <- function (dframe, window_length) {
  # Calculate a sequence of dates corresponding to the starts of each window
  # Args:
  #     dframe - data frame with times_index colum
  #     window_length - the number of time_units per window
  # Returns:
  #     A date sequence corresponding to the start date of each window

  num_windows <- length(unique(dframe$times_index)) - window_length + 1

  if (num_windows <= 1) {
    stop ("window lenght of ", window_length,
          " is longer then the number time periods: ",
          length(unique(dframe$times)))
  }

  seq(from = min(dframe$times_index),
      by = 1,
      length.out = num_windows)
}



get_win_dates <- function(st_date, window_length){
  # Calculate a sequence of dates corresponding to the dates in a window which
  # starts at st_date
  # Args:
  #     st_date - a date, corresponding to the first date of the window
  #     window_length - the number of time_units per window
  # Returns:
  #     A date sequence corresponding to each date in the window

  seq(st_date, by = 1, length.out = window_length)
}




#' Geometric mean
#'
#' Calculate the geometric mean of a vector of numbers
#'
#' @param x an R numerical object
#' @param na.rm  a logical value indicating whether NA values should be
#' stripped before the computation proceeds.
#' @return If all values in x are numeric class, a single numeric
#' class value is returned.
#' @examples
#' x <- c(0:10, 50)
#' gm_mean(x)
#'
#' @export
gm_mean <- function(x, na.rm = TRUE){
  # Implementation Stolen from here:
  # https://stackoverflow.com/questions/2602583/geometric-mean-is-there-a-built-in

  # This is a safer implementation than using PRODUCT () as floating point
  # errors are vrey likely when using PRODUCT () for many large or small numbers
  exp(sum(log(x[x > 0]), na.rm = na.rm) / length(x))
}



#' Index levels to Price movements
#'
#' Convert a series of index levels to period by period price movements.
#'
#' @param index an R vector of numeric values with no NA's
#' @return an R vector of numeric values with lenght 1 less than that of
#' index. The length is one shorter than the input as there is movement for the
#' first value - as it has nothing to refer to.
#' If index contains any NA values an error is thrown
#' @examples
#' index <- runif(10)*1:10
#' index_2_movement(index)
#' @export
index_2_movement <- function(index){
  # Check for any NA values
  if (any(is.na(index))){
    stop("the index supplied contains an NA value")
  }

  # Divide each index by the following value
  temp <- lead(index) / index

  # Remove the last value which is now an NA
  head (temp, -1)


}

#' Price movements Index levels
#'
#' Convert a series of Price movements to an index series
#'
#' @param movement an R vector of numeric values with no NA's
#' @return an R vector of numeric values with lenght 1 greater than that
#' of movement. The length is one longer than the input as there a reference
#' value of 1 is inserted as the level for the first time period.
#' If movement contains any NA values an error is thrown.
#' @examples
#' movement <- runif(10)
#' movement_2_index(movement)
#' @export
movement_2_index <- function(movement){


  # Check for any NA values
  if (any(is.na(movement))){
    stop("the index supplied contains an NA value")
  }

  cumprod(c(1, movement))
}



