# signif_column works ---------------------------------------------------

test_that(
  desc = "signif_column works",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # for reproducibility
    set.seed(123)

    # dataframe with p-values
    p.data <- cbind.data.frame(
      x = 1:5,
      y = 1,
      p.value = c(0.1, 0.5, 0.00001, 0.05, 0.01)
    )

    df3 <- signif_column(
      data = p.data,
      p = p.value,
      messages = FALSE
    )

    # testing
    set.seed(123)
    testthat::expect_identical(names(df3), c("x", "y", "p.value", "significance"))
    testthat::expect_equal(dim(df3), c(5L, 4L))
  }
)
