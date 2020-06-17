#' @title Format *p*-values for creating labels from them for graphics
#' @name p_value_formatter
#'
#' @inheritParams signif_column
#' @inheritParams specify_decimal_p
#'
#' @importFrom dplyr rowwise mutate ungroup case_when
#'
#' @examples
#' # preparing a new dataframe
#' df <-
#'   cbind.data.frame(
#'     x = 1:5,
#'     y = 1,
#'     p.value = c(0.1, 0.5, 0.00001, 0.05, 0.01)
#'   )
#'
#' # prepare label
#' p_value_formatter(data = df, k = 4L)
#' @export

p_value_formatter <- function(data, k = 3L, ...) {
  data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(p.value.formatted = specify_decimal_p(x = p.value, k = k, p.value = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      p.value.formatted = dplyr::case_when(
        p.value.formatted == "< 0.001" ~ "<= 0.001",
        TRUE ~ paste("==", p.value.formatted, sep = " ")
      )
    )
}
