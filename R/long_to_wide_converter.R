#' @title Converts dataframe from long/tidy to wide format with `NA`s removed
#' @name long_to_wide_converter
#' @description This conversion is helpful mostly for repeated measures design.
#'
#' @param data A dataframe (or a tibble) from which variables specified are to
#'   be taken. A matrix or tables will **not** be accepted.
#' @param x The grouping variable from the dataframe `data`.
#' @param y The response (a.k.a. outcome or dependent) variable from the
#'   dataframe `data`.
#' @param paired Logical that decides whether the experimental design is
#'   repeated measures/within-subjects or between-subjects. The default is
#'   `FALSE`.
#' @param spread Logical that decides whether the dataframe needs to be
#'   converted from long/tidy to wide (default: `TRUE`), or the data needs to be
#'   returned as it is but with the `NA`s removed.
#' @param ... Currently ignored.
#'
#' @importFrom rlang :=
#' @importFrom dplyr n row_number select mutate mutate_at group_by ungroup
#' @importFrom tidyr pivot_longer
#'
#' @return A dataframe in the wide (or Cartesian) format.
#'
#' @examples
#' \donttest{
#' long_to_wide_converter(
#'   data = iris_long,
#'   x = condition,
#'   y = value,
#'   paired = TRUE
#' )
#' }
#' @export

# function body
long_to_wide_converter <- function(data,
                                   x,
                                   y,
                                   paired = TRUE,
                                   spread = TRUE,
                                   ...) {
  # make sure both quoted and unquoted arguments are allowed
  c(x, y) %<-% c(rlang::ensym(x), rlang::ensym(y))

  # initial cleanup
  data %<>%
    dplyr::select(.data = ., {{ x }}, {{ y }}) %>%
    dplyr::mutate(.data = ., {{ x }} := droplevels(as.factor({{ x }}))) %>%
    as_tibble(.) %>%
    dplyr::group_by(.data = ., {{ x }}) %>%
    dplyr::mutate(.data = ., rowid = dplyr::row_number()) %>%
    dplyr::ungroup(.)

  # NA removal
  if (isTRUE(paired)) {
    data %<>% dplyr::anti_join(x = ., y = dplyr::filter(., is.na({{ y }})), by = "rowid")
  } else {
    data %<>% tidyr::drop_na(.)
  }

  # convert to wide?
  if (isTRUE(spread)) {
    data %>%
      tidyr::pivot_wider(
        data = .,
        names_from = {{ x }},
        values_from = {{ y }}
      )
  } else {
    data
  }
}
