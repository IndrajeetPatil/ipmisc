#'
#' @title custom function to get summary for multiple variables for all grouping variable levels
#' @name summary_skim
#' @return dataframe with summary statistics
#' @author Indrajeet Patil
#'
#' @param data dataframe
#' @param grouping.vars list of grouping variables
#' @param measures list variables for which summary needs to computed
#'
#' @import dplyr
#' @import rlang
#'
#' @importFrom skimr skim_to_wide
#' @importFrom tibble as_data_frame
#' @importFrom purrr map
#' @importFrom tidyr nest
#' @importFrom tidyr unnest
#'
#' @examples
#' library(datasets)
#' summary_skim(data = mtcars, grouping.vars = c(am, cyl), measures = c(wt, mpg))
#'
#' @export

summary_skim <- function(data,
                         grouping.vars = ...,
                         measures = ...) {
  # getting the dataframe ready
  df <-
    dplyr::select(.data = data,
                  !!rlang::enquo(arg = grouping.vars),
                  !!rlang::enquo(arg = measures))

  # summarize by specified grouping variables
  df_nest <- df %>%
    dplyr::group_by(.data = ., !!!as.list(rlang::quo_squash(quo = rlang::enquo(arg = grouping.vars)))[-1]) %>%
    tidyr::nest(data = .)

  # computing summary
  df_summary <- df_nest %>%
    dplyr::mutate(
      .data = .,
      summary = data %>% # 'data' variable is automatically created by tidyr::nest function
        purrr::map(.x = .,
                   .f = skimr::skim_to_wide)
    ) %>%
    dplyr::select(.data = ., -data) %>% # removing the redudant data column
    dplyr::mutate(
      .data = .,
      summary = summary %>%
        purrr::map(
          .x = .,
          .f = dplyr::select,
          -hist,
          -missing,
          -complete,
          -type
        )
    ) %>%
    tidyr::unnest(data = .) %>% # unnesting the data
    tibble::as_data_frame(x = .) # converting to tibble dataframe

  # changing class of summary variables
  df_summary <-
    df_summary %>%
    dplyr::mutate_at(
      .tbl = .,
      .vars = dplyr::vars(n:max),
      .funs = ~ as.numeric(as.character(.)) # change summary variables to numeric
    ) %>%
    dplyr::mutate_if(.tbl = .,
                     .predicate = is.character,
                     .funs = as.factor) # change grouping variables to factors

  return(df_summary)

}
