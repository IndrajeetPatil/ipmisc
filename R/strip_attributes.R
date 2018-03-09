#' @title Remove attributes from a data.frame
#' @name strip_attributes
#' @author Indrajeet Patil
#' @description Strips attributes off of a data frame that come with a merMod model.frame
#' @return a tibble with variable names cleaned to remove all attributes except for
#' names, row.names, and class
#'
#' @param data a dataframe
#'
#' @importFrom tibble as_data_frame
#'
#' @examples
#' library(datasets)
#' strip_attributes(data = iris)
#'
#' @export

strip_attributes <- function(data) {
  # get all the names of the attributes present in the dataframe
  attr <- names(attributes(data))
  # some attributes worth retaining in the dataframes
  good <- c("names", "row.names", "class")
  # remove all the attributes that are *not* good
  for (i in attr[!attr %in% good]) {
    attr(data, i) <- NULL
  }
  # convert it to tibble
  data <- tibble::as_data_frame(data)
  # return the stripped dataframe
  return(data)
}
