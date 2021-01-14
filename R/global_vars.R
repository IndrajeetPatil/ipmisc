# defining global variables and functions to appease R CMD Check

utils::globalVariables(
  names = c(".", "rowid"),
  package = "ipmisc",
  add = FALSE
)
