#'
#' @title Custom function to get descriptive statistics for multiple numeric variables for all grouping variable levels
#' @name summary_skim
#' @author Indrajeet Patil
#' @return Dataframe with descriptive statistics for numeric variables (n, mean, sd, median, min, max)
#'
#' @param data dataframe
#' @param grouping.vars list of grouping variables
#' @param measures list variables for which summary needs to computed (only *numeric* variables should be entered)
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
#' summary_skim(data = mtcars, grouping.vars = am, measures = wt)
#'
#' @export

summary_skim <- function(data,
                         grouping.vars,
                         measures) {
  # check how many variables were entered for this grouping variable
  grouping.vars <-
    as.list(rlang::quo_squash(rlang::enquo(grouping.vars)))
  grouping.vars <-
    if (length(grouping.vars) == 1) {
      # e.g., in mtcars dataset, grouping.vars = am
      grouping.vars
    } else {
      # e.g., in mtcars dataset, grouping.vars = c(am, cyl)
      grouping.vars[-1]
    }

  # getting the dataframe ready
  df <- dplyr::select(.data = data,
                      !!!grouping.vars,
                      !!rlang::enquo(measures))

  # creating a nested dataframe
  df_nest <- df %>%
    dplyr::group_by(!!!grouping.vars) %>%
    tidyr::nest(data = .)

  # computing summary
  df_summary <- df_nest %>%
    dplyr::mutate(
      .data = .,
      summary = data %>% # 'data' variable is automatically created by tidyr::nest function
        purrr::map(.x = .,
                   .f = skimr::skim_to_wide)
    )

  # tidying up the skimr output by removing unnecessary information and renaming certain columns
  df_summary <- df_summary %>%
    dplyr::select(.data = ., -data) %>% # removing the redudant data column
    dplyr::mutate(
      .data = .,
      summary = summary %>%
        purrr::map(
          .x = .,
          .f = dplyr::select,
          -type,
          -missing,
          -complete,
          -hist
        )
    ) %>%
    tidyr::unnest(data = .) %>% # unnesting the data
    tibble::as_data_frame(x = .) # converting to tibble dataframe

  # changing class of summary variables
  df_summary <-
    df_summary %>%
    dplyr::mutate_at(
      .tbl = .,
      .vars = dplyr::vars(n, mean, sd, p0, p25, median, p75, p100),
      .funs = ~ as.numeric(as.character(.)) # change summary variables to numeric
    ) %>%
    dplyr::rename(.data = ., min = p0, max = p100) %>% # renaming columns to minimum and maximum
    dplyr::mutate_if(.tbl = .,
                     .predicate = is.character,
                     .funs = as.factor) # change grouping variables to factors

  return(df_summary)

}
