#' @title Converts dataframe from long/tidy to wide format with `NA`s removed
#' @name long_to_wide_converter
#' @description This conversion is helpful mostly for repeated measures design.
#'
#' @param data A dataframe (or a tibble) from which variables specified are to
#'   be taken. A matrix or tables will **not** be accepted.
#' @param x The grouping variable from the dataframe `data`.
#' @param y The response (a.k.a. outcome or dependent) variable from the
#'   dataframe `data`.
#' @param subject.id In case of repeated measures design (`paired = TRUE`,
#'   i.e.), this argument specifies the subject or repeated measures id. Note
#'   that if this argument is `NULL` (which is the default), the function
#'   assumes that the data has already been sorted by such an id by the user and
#'   creates an internal identifier. So if your data is **not** sorted and you
#'   leave this argument unspecified, the results can be inaccurate.
#' @param paired Logical that decides whether the experimental design is
#'   repeated measures/within-subjects or between-subjects. The default is
#'   `FALSE`.
#' @param spread Logical that decides whether the dataframe needs to be
#'   converted from long/tidy to wide (default: `TRUE`), or the data needs to be
#'   returned as it is but with the `NA`s removed.
#' @param ... Currently ignored.
#'
#' @importFrom dplyr row_number select mutate group_by ungroup arrange
#' @importFrom tidyr pivot_longer
#'
#' @return A dataframe in the wide (or Cartesian) format.
#'
#' @examples
#' # for reproducibility
#' library(ipmisc)
#' set.seed(123)
#'
#' # repeated measures design
#' long_to_wide_converter(
#'   data = bugs_long,
#'   x = condition,
#'   y = desire,
#'   subject.id = subject,
#'   paired = TRUE
#' )
#'
#' # independent measures design
#' long_to_wide_converter(
#'   data = ggplot2::msleep,
#'   x = vore,
#'   y = brainwt,
#'   paired = FALSE,
#'   spread = FALSE
#' )
#' @export

# function body
long_to_wide_converter <- function(data,
                                   x,
                                   y,
                                   subject.id = NULL,
                                   paired = TRUE,
                                   spread = TRUE,
                                   ...) {
  # make sure both quoted and unquoted arguments are allowed
  c(x, y) %<-% c(rlang::ensym(x), rlang::ensym(y))
  subject.id <- if (!rlang::quo_is_null(rlang::enquo(subject.id))) rlang::ensym(subject.id)

  # initial cleanup
  data %<>%
    dplyr::select(.data = ., {{ x }}, {{ y }}, rowid = {{ subject.id }}) %>%
    dplyr::mutate(.data = ., {{ x }} := droplevels(as.factor({{ x }}))) %>%
    as_tibble(.) %>%
    dplyr::arrange(.data = ., {{ x }})

  # if `subject.id` wasn't provided, create one for internal usage
  if (!"rowid" %in% names(data)) {
      data %<>%
        dplyr::group_by(.data = ., {{ x }}) %>% # for paired designs
        dplyr::mutate(.data = ., rowid = dplyr::row_number()) %>%
        dplyr::ungroup(.)
  }

  # NA removal
  if (isTRUE(paired)) {
    data %<>% dplyr::anti_join(x = ., y = dplyr::filter(., is.na({{ y }})), by = "rowid")
  } else {
    data %<>% tidyr::drop_na(.)
  }

  # convert to wide?
  if (isTRUE(spread)) {
    data %<>% tidyr::pivot_wider(data = ., names_from = {{ x }}, values_from = {{ y }})
  }

  # final clean-up
  data %>%
    dplyr::arrange(.data = ., rowid) %>%
    dplyr::select(.data = ., rowid, dplyr::everything())
}
