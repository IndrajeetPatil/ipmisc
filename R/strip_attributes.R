#' @title Remove attributes from a data.frame
#' @name strip_attributes
#' @description Strips attributes off of a data frame that come with a merMod model.frame
#' @param data a data.frame
#' @return a data frame with variable names cleaned to remove all attributes except for
#' names, row.names, and class

strip_attributes <- function(data) {
  attr <- names(attributes(data))
  good <- c("names", "row.names", "class")
  for (i in attr[!attr %in% good]) {
    attr(data, i) <- NULL
  }
  return(data)
}
