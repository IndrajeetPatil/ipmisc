# defining global variables and functions to appease R CMD Check

utils::globalVariables(
  names = c(".", "rowid", "isanoutlier"),
  package = "ipmisc",
  add = FALSE
)
