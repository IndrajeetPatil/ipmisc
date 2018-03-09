#'
#' @title creating a column with significance labels
#' @name signif_column
#' @aliases signif_column
#' @author Indrajeet Patil
#' @description This function will add a new column to a dataframe containing p-values
#' @return Returns the originally entered object (either a vector or a dataframe) in tibble format with
#' an additional column corresponding to statistical significance.
#'
#' @param data data frame from which variables specified are preferentially to be taken
#' @param p the column containing p-values
#'
#' @import dplyr
#'
#' @importFrom broom tidy
#' @importFrom crayon red
#' @importFrom crayon blue
#' @importFrom rlang enquo
#' @importFrom stats lm
#' @importFrom tibble as_data_frame
#'
#' @examples
#' library(datasets)
#' library(stats)
#' library(broom)
#' mtcars %>%
#' stats::lm(mpg ~ wt + qsec, .) %>%
#' broom::tidy(.) %>%
#' ipmisc::signif_column(data = ., p = p.value)
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
    "p25",
    "p50",
    "p75",
    "sd",
    "significance",
    "type"
  )
)

signif_column <- function(data = NULL, p) {
  # storing variable name to be assigned later
  p_lab <- colnames(dplyr::select(.data = data,
                                  !!rlang::enquo(p)))
  # if dataframe is provided
  if (!is.null(data)) {
    df <-
      dplyr::select(.data = data,
                    # column corresponding to p-values
                    p = !!rlang::enquo(p),
                    dplyr::everything())
  } else {
    # if only vector is provided
    df <-
      base::cbind.data.frame(p = p) # column corresponding to p-values
  }

  # make sure the p-value column is numeric; if not, convert it to numeric and give a warning to the user
  if (!is.numeric(df$p)) {
    df$p <- as.numeric(as.character(df$p))
    base::message(cat(
      crayon::red("Warning: "),
      crayon::blue(
        "Entered p-values were not numeric variables, so ipmisc has converted them to numeric"
      )
    ))
  }
  # add new significance column based on standard APA guidelines for describing different levels of significance
  df <- df %>%
    dplyr::mutate(
      .data = .,
      significance = dplyr::case_when(
        # first condition
        p >= 0.050 ~ "ns",
        # second condition
        p < 0.050 &
          p >= 0.010 ~ "*",
        # third condition
        p < 0.010 &
          p >= 0.001 ~ "**",
        # fourth condition
        p < 0.001 ~ "***"
      )
    ) %>%
    tibble::as_data_frame(x = .) # convert to tibble dataframe
  # change back from the generic p-value to the original name that was provided by the user for the p-value
  if (!is.null(data)) {
    # reordering the dataframe
    df <- df %>%
      dplyr::select(.data = ., -p, -significance, dplyr::everything())
    # renaming the p-value variable with the name provided by the user
    colnames(df)[which(names(df) == "p")] <- p_lab
  }
  # return the final tibble dataframe
  return(df)
}
