#' Algorithmia Wrapper for FEWS
#'
#' @param input_json an input json. See ?FEWS for the details of the JSON
#' @return see ?FEWS for return details. This function will retun the exact same
#' results, except in json format rather than a list
#' @export
#' @import jsonlite
FEWS_algorithmia <- function(input_json){
  # This is a wrapper for the algorithmia implementation of the FEWS code
  # It takes a json and unpacks it and then calls the function as normal
  # It then wraps the output back up into a json to return it

  input_list <- fromJSON(input_json)

  fews_op <- FEWS(times = input_list$times,
                  logprice = input_list$logprice,
                  id = input_list$id,
                  window_length = input_list$window_length,
                  weight = input_list$weight,
                  splice_pos = input_list$splice_pos,
                  num_cores = NULL)

  toJSON(fews_op)

}
