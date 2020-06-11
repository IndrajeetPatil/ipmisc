# long_to_wide_converter works ---------------------------------------------

testthat::test_that(
  desc = "long_to_wide_converter works - spread true",
  code = {
    # ----------------------- data without NAs ------------------------------

    # within-subjects
    set.seed(123)
    df1 <-
      long_to_wide_converter(
        data = iris_long,
        x = "condition",
        y = value
      )

    testthat::expect_equal(dim(df1), c(150L, 5L))

    # between-subjects
    set.seed(123)
    df2 <-
      long_to_wide_converter(
        data = mtcars,
        x = am,
        y = "wt",
        paired = FALSE
      )

    testthat::expect_equal(dim(df2), c(19L, 3L))

    # -------------------------- data with NAs ------------------------------

    # within-subjects
    set.seed(123)
    df3 <-
      long_to_wide_converter(
        data = bugs_long,
        x = condition,
        y = desire,
        paired = TRUE
      )

    testthat::expect_equal(dim(df3), c(88L, 5L))

    # between-subjects
    set.seed(123)
    df4 <-
      long_to_wide_converter(
        data = ggplot2::msleep,
        x = vore,
        y = brainwt,
        paired = FALSE
      )

    testthat::expect_equal(dim(df4), c(26L, 5L))
  }
)


testthat::test_that(
  desc = "long_to_wide_converter works - spread false",
  code = {
    # ----------------------- data without NAs ------------------------------

    # within-subjects
    set.seed(123)
    df1 <-
      long_to_wide_converter(
        data = iris_long,
        x = "condition",
        y = value,
        spread = FALSE
      )

    testthat::expect_equal(dim(df1), c(600L, 3L))

    # between-subjects
    set.seed(123)
    df2 <-
      long_to_wide_converter(
        data = mtcars,
        x = am,
        y = "wt",
        paired = FALSE,
        spread = FALSE
      )

    testthat::expect_equal(dim(df2), c(32L, 3L))

    # -------------------------- data with NAs ------------------------------

    # within-subjects
    set.seed(123)
    df3 <-
      long_to_wide_converter(
        data = bugs_long,
        x = condition,
        y = desire,
        paired = TRUE,
        spread = FALSE
      )

    testthat::expect_equal(dim(df3), c(352L, 3L))

    # between-subjects
    set.seed(123)
    df4 <-
      long_to_wide_converter(
        data = ggplot2::msleep,
        x = vore,
        y = brainwt,
        paired = FALSE,
        spread = FALSE
      )

    testthat::expect_equal(dim(df4), c(51L, 3L))
  }
)
