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
#' @param nboot Number of bootstrap samples for confidence intervals for partial eta-squared.
#'
#' @import dplyr
#'
#' @importFrom sjstats eta_sq
#' @importFrom stats anova
#' @importFrom stats na.omit
#' @importFrom stats lm
#' @importFrom tibble as_data_frame
#' @importFrom tibble rownames_to_column
#'
#' @examples
#' library(datasets)
#' library(stats)
#' x <- stats::lm(formula = wt ~ mpg*cyl, data = mtcars)
#' ipmisc::partialeta_sq_ci(lm_object = x, conf.level = 0.95)
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
    "type",
    "conf.high",
    "conf.low",
    "partial.etasq",
    "term"
  )
)

# defining the function body
partialeta_sq_ci <-
  function(lm_object,
           conf.level = 0.95,
           nboot = 1000) {
    # get the linear model object and turn it into a matrix and turn row names into a variable called "effect"
    # compute partial eta-squared for each effect
    # add additional columns containing data and formula that was used to create these effects
    # details from the anova results
    aov_df <-
      as.data.frame(as.matrix(stats::anova(object = lm_object))) %>%
      tibble::rownames_to_column(df = ., var = "effect")
    # create a new column for residual degrees of freedom
    aov_df$df2 <- aov_df$Df[aov_df$effect == "Residuals"]

    aov_df <- aov_df %>%
      dplyr::select(.data = .,
                    -c(base::grep(pattern = "Sq", x = names(.)))) %>%
      dplyr::rename(.data = .,
                    df1 = Df) %>% # rename to something more meaningful and tidy
      stats::na.omit(.) # remove NAs, which would remove the row containing Residuals (redundant at this point)

    # other supplementary information about the results (data and formula used, etc.) can be composed from
    # sjstats::eta_sq function that now provides 95% CI as well

    # creating dataframe of effect size and its CI with sjstats
    etasq_df <- sjstats::eta_sq(
      model = stats::anova(object = lm_object),
      partial = TRUE,
      ci.lvl = conf.level,
      n = nboot
    ) %>%
      dplyr::rename(
        .data = .,
        effect = term,
        effsize = partial.etasq,
        LL = conf.low,
        UL = conf.high
      )

    # combining the dataframes (erge the two preceding pieces of information by the common element of Effect
    combined_df <- dplyr::left_join(x = aov_df,
                                    y = etasq_df,
                                    by = "effect") %>% # adding information about dataframe and formula call
      dplyr::mutate(
        .data = .,
        data = as.character(lm_object$call[3]),
        formula = as.character(lm_object$call[2])
      ) %>% # reordering columns
      dplyr::select(.data = .,
                    data,
                    formula,
                    effect,
                    `F value`,
                    df1,
                    df2,
                    `Pr(>F)`,
                    effsize,
                    LL,
                    UL) %>%
      tibble::as_data_frame(x = .)

    # returning the final dataframe
    return(combined_df)
  }
