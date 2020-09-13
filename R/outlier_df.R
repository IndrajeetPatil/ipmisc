#' @title Adding a column to dataframe describing outlier status
#' @name outlier_df
#'
#' @inheritParams long_to_wide_converter
#' @param outlier.label Label to put on the outliers that have been tagged. This
#'   can't be the same as x argument.
#' @param outlier.coef Coefficient for outlier detection using Tukey's method.
#'   With Tukey's method, outliers are below (1st Quartile) or above (3rd
#'   Quartile) `coef` times the Inter-Quartile Range (IQR) (Default: `1.5`).
#' @param ... Additional arguments.
#'
#' @return The dataframe entered as `data` argument is returned with two
#'   additional columns: `isanoutlier` and `outlier` denoting which observation
#'   are outliers and their corresponding labels.
#'
#' @importFrom rlang enquo ensym
#' @importFrom dplyr group_by mutate ungroup
#'
#' @examples
#' # adding column for outlier and a label for that outlier
#' outlier_df(
#'   data = morley,
#'   x = Expt,
#'   y = Speed,
#'   outlier.label = Run,
#'   outlier.coef = 2
#' ) %>%
#'   dplyr::arrange(outlier)
#' @export

# function body
outlier_df <- function(data,
                       x,
                       y,
                       outlier.label,
                       outlier.coef = 1.5,
                       ...) {
  # make sure both quoted and unquoted arguments are allowed
  c(x, y, outlier.label) %<-% c(rlang::ensym(x), rlang::ensym(y), rlang::ensym(outlier.label))

  # add a logical column indicating whether a point is or is not an outlier
  data %>%
    dplyr::group_by(.data = ., {{ x }}) %>%
    dplyr::mutate(
      isanoutlier = ifelse(check_outlier({{ y }}, outlier.coef), TRUE, FALSE),
      outlier = ifelse(isanoutlier, {{ outlier.label }}, NA)
    ) %>%
    dplyr::ungroup(.)
}

#' @title Finding the outliers in the dataframe using Tukey's interquartile
#'   range rule
#' @name check_outlier
#' @importFrom stats quantile
#' @noRd

# defining function to detect outliers
check_outlier <- function(var, coef = 1.5) {
  # compute the quantiles
  quantiles <- stats::quantile(x = var, probs = c(0.25, 0.75), na.rm = TRUE)

  # compute the interquartile range
  IQR <- quantiles[2] - quantiles[1]

  # check for outlier and output a logical
  (var < (quantiles[1] - coef * IQR)) | (var > (quantiles[2] + coef * IQR))
}
