# p_value_formatter works ----------------------------------------

test_that(
  desc = "p_value_formatter works",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # for reproducibility
    set.seed(123)

    # dataframe with p-values
    p.data <-
      cbind.data.frame(
        x = 1:5,
        y = 1,
        p.value = c(0.1, 0.5, 0.00001, 0.05, 0.01)
      )

    df <-
      p_value_formatter(
        data = p.data,
        messages = FALSE
      )

    # testing
    set.seed(123)
    testthat::expect_identical(
      names(df),
      c("x", "y", "p.value", "p.value.formatted")
    )
    testthat::expect_equal(dim(df), c(5L, 4L))
    testthat::expect_identical(
      df$p.value.formatted,
      c("== 0.100", "== 0.500", "<= 0.001", "== 0.050", "== 0.010")
    )
  }
)
