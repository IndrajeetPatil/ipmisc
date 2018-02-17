#'
#' @title Confidence intervals for Partial Eta Squared
#' @name partialeta_sq_ci
#' @author Indrajeet Patil
#' @description This function will convert a linear model object to a dataframe containing statistical details
#' for all effects along with partial eta-squared effect size and its confidence interval.
#' @return A dataframe with results from `stats::lm()` with partial eta-squared and its confidence interval.
#'
#' @param lm_object stats::lm linear model object
#' @param conf.level Level of confidence for the confidence interval
#'
#' @import plyr
#' @import dplyr
#'
#' @importFrom sjstats eta_sq
#' @importFrom data.table setDT
#' @importFrom stats anova
#' @importFrom stats na.omit
#' @importFrom tibble as_data_frame
#' @importFrom tibble rownames_to_column
#'
#' @examples
#' library(datasets)
#' library(stats)
#' x <- stats::lm(iris$Sepal.Length ~ iris$Species)
#' partialeta_sq_ci(lm_object = x, conf.level = 0.95)
#'
#' @export

partialeta_sq_ci <- function(lm_object, conf.level = 0.95) {
  # get the linear model object and turn it into a matrix and turn row names into a variable called "effect"
  # compute partial eta-squared for each effect
  # add additional columns containing data and formula that was used to create these effects
  # details from the anova results
  aov_df <-
    as.data.frame(as.matrix(stats::anova(object = lm_object))) %>%
    tibble::rownames_to_column(df = ., var = "effect")

  # other supplementary information about the results (data and formula used, etc.)
  supp_df <- cbind.data.frame(
    "effsize" = sjstats::eta_sq(
      model = stats::anova(object = lm_object),
      partial = TRUE
    ),
    "data" = as.character(lm_object$call[3]),
    "formula" = as.character(lm_object$call[2])
  ) %>%
    tibble::rownames_to_column(df = ., var = "effect")

  # combining the dataframes (erge the two preceding pieces of information by the common element of Effect)
  x <-
    dplyr::left_join(x = aov_df,
                     y = supp_df,
                     by = "effect")
  # create a new column for residual degrees of freedom
  x$df2 <- x$Df[x$effect == "Residuals"]
  # remove sum of squares columns since they will not be useful
  x <- x %>%
    dplyr::select(.data = .,
                  -c(base::grep(pattern = "Sq", x = names(x)))) %>%
    dplyr::rename(.data = .,
                  df1 = Df,
                  F.value = `F value`) %>% # rename to something more meaningful and tidy
    stats::na.omit(.) # remove NAs, which would remove the row containing Residuals (redundant at this point)

  # rearrange the columns
  x <-
    x[, c("F.value",
          "df1",
          "df2",
          "effect",
          "effsize",
          "Pr(>F)",
          "data",
          "formula")]
  # convert the effect into a factor
  x$effect <- as.factor(x$effect)
  # for each type of effect, compute partial eta-squared confidence intervals, which would return a list
  ci_df <-
    plyr::dlply(
      .data = x,
      .variables = .(effect),
      .fun = function(data)
        apaTables::get.ci.partial.eta.squared(
          F.value = data$F.value,
          df1 = data$df1,
          df2 = data$df2,
          conf.level = conf.level
        )
    )

  # get elements from the effect size confidence intervals list into a neat dataframe
  ci_df <-
    plyr::ldply(
      .data = ci_df,
      .fun = function(x)
        cbind("LL" = x[[1]],
              "UL" = x[[2]])
    )

  # merge the dataframe containing effect sizes with the dataframe containing rest of the information
  # and convert to tibble
  effsize_ci <- tibble::as_data_frame(base::merge(x = x,
                                                  y = ci_df,
                                                  by = "effect"))
  # returning the final dataframe
  return(effsize_ci)

}
