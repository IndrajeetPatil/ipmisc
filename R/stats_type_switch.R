#' @name stats_type_switch
#' @title Switch type of statistics.
#' @description Relevant mostly for `ggstatsplot` and `statsExpressions`
#'   packages, where there are four types of statistics are supported:
#'   parametric, non-parametric, robust, and Bayesian. This switch function
#'   converts strings entered by users to a common pattern.
#'
#' @param type Character string describing the type of statistics.
#'
#' @importFrom dplyr case_when
#'
#' @examples
#' stats_type_switch("p")
#' stats_type_switch("bf")
#' @export

stats_type_switch <- function(type) {
  dplyr::case_when(
    grepl("^p", type, TRUE) ~ "parametric",
    grepl("^n|^s", type, TRUE) ~ "nonparametric", # s is for Spearman's rho
    grepl("^r", type, TRUE) ~ "robust",
    grepl("^b", type, TRUE) ~ "bayes",
    TRUE ~ "parametric"
  )
}
