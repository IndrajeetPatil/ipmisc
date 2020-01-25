#' @title Converts long-format dataframe to wide-format dataframe
#' @name long_to_wide_converter
#' @author \href{https://github.com/IndrajeetPatil}{Indrajeet Patil}
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
#' @param ... Currently ignored.
#'
#' @importFrom rlang :=
#' @importFrom dplyr n row_number select mutate mutate_at group_by ungroup
#' @importFrom tidyr spread
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
                                   ...) {
  # make sure both quoted and unquoted arguments are allowed
  c(x, y) %<-% c(rlang::ensym(x), rlang::ensym(y))

  # creating a dataframe
  data %<>%
    dplyr::select(.data = ., {{ x }}, {{ y }}) %>%
    dplyr::mutate(.data = ., {{ x }} := droplevels(as.factor({{ x }}))) %>%
    tibble::as_tibble(x = .)

  # figuring out number of levels in the grouping factor
  x_n_levels <- nlevels(data %>% dplyr::pull({{ x }}))[[1]]

  # wide format
  data_wide <-
    data %>%
    dplyr::filter(.data = ., !is.na({{ x }})) %>%
    dplyr::group_by(.data = ., {{ x }}) %>%
    dplyr::mutate(.data = ., rowid = dplyr::row_number()) %>%
    dplyr::ungroup(x = .) %>%
    dplyr::filter(.data = ., !is.na({{ y }}))

  # clean up for repeated measures design
  if (isTRUE(paired)) {
    data_wide %<>%
      dplyr::group_by(.data = ., rowid) %>%
      dplyr::mutate(.data = ., n = dplyr::n()) %>%
      dplyr::ungroup(x = .) %>%
      dplyr::filter(.data = ., n == x_n_levels) %>%
      dplyr::select(.data = ., {{ x }}, {{ y }}, rowid)
  }

  # spreading the columns of interest
  data_wide %<>%
    tidyr::spread(
      data = .,
      key = {{ x }},
      value = {{ y }},
      convert = TRUE
    )

  # return the dataframe in wide format
  return(data_wide)
}
