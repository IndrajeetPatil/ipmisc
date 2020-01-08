#' @title Creating a new column with significance labels
#' @name signif_column
#' @author Indrajeet Patil
#' @description This function will add a new column with significance labels to
#'   a dataframe containing *p*-values.
#' @return Returns the dataframe in tibble format with an additional column
#'   corresponding to APA-format statistical significance labels.
#'
#' @param data Data frame from which variables specified are preferentially to
#'   be taken.
#' @param p The column containing *p*-values.
#' @param ... Currently ignored.
#'
#' @importFrom dplyr mutate case_when
#'
#' @family helper_stats
#'
#' @examples
#' # preparing a new dataframe
#' df <- cbind.data.frame(
#'   x = 1:5,
#'   y = 1,
#'   p.value = c(0.1, 0.5, 0.00001, 0.05, 0.01)
#' )
#'
#' # dataframe with significance column
#' signif_column(data = df, p = p.value)
#' @export

# function body
signif_column <- function(data, p, ...) {
  # add new significance column based on standard APA guidelines
  data %<>%
    dplyr::mutate(
      .data = .,
      significance = dplyr::case_when(
        {{ p }} >= 0.050 ~ "ns",
        {{ p }} < 0.050 & {{ p }} >= 0.010 ~ "*",
        {{ p }} < 0.010 & {{ p }} >= 0.001 ~ "**",
        {{ p }} < 0.001 ~ "***"
      )
    ) %>% # convert to tibble dataframe
    tibble::as_tibble(.)
}
