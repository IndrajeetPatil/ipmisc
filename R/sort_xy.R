#' @name sort_xy
#' @title Sorting `y` column in data by `x`.
#'
#' @param sort If `"ascending"` (default), `x`-variable factor levels will be
#'   sorted based on increasing values of `y`-variable. If `"descending"`, the
#'   opposite. If `"none"`, no sorting will happen.
#' @inheritParams long_to_wide_converter
#' @inheritParams forcats::fct_reorder
#'
#' @importFrom forcats fct_reorder
#' @importFrom dplyr mutate
#'
#' @examples
#' sort_xy(ggplot2::msleep, vore, brainwt, sort = "ascending")
#' @export

# function body
sort_xy <- function(data,
                    x,
                    y,
                    sort = "none",
                    .fun = mean,
                    ...) {
  # make sure both quoted and unquoted arguments are allowed
  c(x, y) %<-% c(rlang::ensym(x), rlang::ensym(y))

  # reorder
  data %>%
    dplyr::mutate(
      .data = .,
      {{ x }} := forcats::fct_reorder(
        .f = {{ x }},
        .x = {{ y }},
        .fun = .fun,
        na.rm = TRUE,
        .desc = ifelse(sort == "ascending", FALSE, TRUE)
      )
    )
}
