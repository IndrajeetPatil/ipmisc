#'
#' @title Transposing a dataframe while preserving row and column names
#' @name transpose_df
#' @author Indrajeet Patil
#' @description Although functions like `data.table::transpose()` or `base::t()` can transpose dataframes well, they
#' don't preserve row and column names. The current function transposes dataframes while preserving row and column names.
#' @return A transposed dataframes with row and column names.
#'
#' @param df Dataframe to tranpose.
#' @param var Name of column to use for rownames.
#' @param ignore.empty Default is `FALSE`. `TRUE` will ignore length-0 list elements.
#'
#' @importFrom data.table transpose
#' @importFrom tibble as_data_frame
#' @importFrom tibble rownames_to_column
#'
#' @examples
#' set.seed(402)
#' data <- cbind.data.frame(x = rnorm(5), y = rnorm(5))
#' transpose_df(df = data)
#' @export

transpose_df <-
  function(df,
             var = "rowname",
             ignore.empty = FALSE) {
    # transpose the dataframe
    t_df <-
      data.table::transpose(
        l = df,
        fill = NA,
        # used to fill shorter list elements so as to return each element of
        # the transposed result of equal lengths
        ignore.empty = ignore.empty
      )
    # assign the row and column names of the original dataframe to the new dataframe
    colnames(t_df) <- rownames(df)
    rownames(t_df) <- colnames(df)
    # convert the row names into a new column and convert it to a tibble dataframe
    t_df <- t_df %>%
      tibble::rownames_to_column(df = ., var = var) %>%
      tibble::as_data_frame(x = .)
    # return the fina, transposed dataframe
    return(t_df)
  }
