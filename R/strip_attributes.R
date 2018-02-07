#' @title Remove attributes from a data.frame
#' @name strip_attributes
#' @author Indrajeet Patil
#'
#' @description Strips attributes off of a data frame that come with a merMod model.frame
#' @param data a data.frame
#' @return a data frame with variable names cleaned to remove all attributes except for
#' names, row.names, and class
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
  # return the stripped dataframe
  return(data)
}
