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
#' library(ipmisc)
#' # if you have multiple variable for each argument
#' summary_skim(data = mtcars, grouping.vars = c(am, cyl), measures = c(wt, mpg))
#' # if you have just one variable per argument
#' summary_skim(data = mtcars, grouping.vars = am, measures = wt)
#'
#' @export

# defining global variables and functions to quient the R CMD check notes
utils::globalVariables(
  c(
    "Df",
    "F value",
    "F.value",
    "LL",
    "Pr(>F)",
    "UL",
    "complete",
    "data",
    "df1",
    "df2",
    "effect",
    "effsize",
    "formula",
    "hist",
    "median",
    "p0",
    "p100",
    "p50",
    "p25",
    "p75",
    "sd",
    "type"
  )
)

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
  df <- dplyr::select(
    .data = data,
    !!!grouping.vars,
    !!rlang::enquo(measures)
  )

  # creating a nested dataframe
  df_nest <- df %>%
    dplyr::group_by(!!!grouping.vars) %>%
    tidyr::nest(data = .)

  # computing summary
  df_summary <- df_nest %>%
    dplyr::mutate(
      .data = .,
      summary = data %>% # 'data' variable is automatically created by tidyr::nest function
        purrr::map(
          .x = .,
          .f = skimr::skim_to_wide
        )
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
          -hist # remove the histograms since they are not that helpful
        )
    ) %>%
    tidyr::unnest(data = .) %>% # unnesting the data
    tibble::as_data_frame(x = .) # converting to tibble dataframe

  # changing class of summary variables
  df_summary <-
    df_summary %>%
    dplyr::mutate_at(
      .tbl = .,
      .vars = dplyr::vars(missing, complete, n, mean, sd, p0, p25, p50, p75, p100),
      .funs = ~ as.numeric(as.character(.)) # change summary variables to numeric
    ) %>%
    dplyr::rename(.data = ., min = p0, median = p50, max = p100) %>% # renaming columns to minimum and maximum
    dplyr::mutate_if(
      .tbl = .,
      .predicate = is.character,
      .funs = as.factor
    ) # change grouping variables to factors (tibble won't have it though)

  # return the summary dataframe
  return(df_summary)
}
