# normality_message is working ---------------------------------------------

testthat::test_that(
  desc = "normality_message is working",
  code = {

    # message
    testthat::expect_output(
      normality_message(x = iris$Sepal.Length, k = 4),
      "0.0102",
      fixed = TRUE
    )

    # stats results
    df <-
      normality_message(
        x = iris$Sepal.Length,
        k = 4,
        output = "stats"
      )

    testthat::expect_equal(df$p.value[[1]], 0.01018116, tolerance = 0.001)
    testthat::expect_equal(df$statistic[[1]], 0.9760903, tolerance = 0.001)
  }
)


# bartlett_message is working ---------------------------------------------

testthat::test_that(
  desc = "bartlett_message is working",
  code = {

    # without NA ------------------------------------------------------

    # message
    testthat::expect_output(
      bartlett_message(
        data = morley,
        x = Expt,
        y = Speed,
        k = 4
      ),
      "Note: Bartlett's test for homogeneity of variances for factor Expt: p-value = 0.0210",
      fixed = TRUE
    )

    # stats results
    df <-
      bartlett_message(
        data = morley,
        x = Expt,
        y = Speed,
        k = 4,
        output = "stats"
      )

    testthat::expect_equal(df$p.value[[1]], 0.02101512, tolerance = 0.001)
    testthat::expect_equal(df$statistic[[1]], 11.55176, tolerance = 0.001)

    # with NA ------------------------------------------------------

    # message
    testthat::expect_output(
      bartlett_message(
        data = ggplot2::msleep,
        x = vore,
        y = sleep_rem,
        k = 4
      ),
      "Note: Bartlett's test for homogeneity of variances for factor vore: p-value = 0.0225",
      fixed = TRUE
    )

    # stats results
    df <-
      bartlett_message(
        data = ggplot2::msleep,
        x = vore,
        y = sleep_rem,
        k = 4,
        output = "stats"
      )

    testthat::expect_equal(df$p.value[[1]], 0.0225323, tolerance = 0.001)
    testthat::expect_equal(df$statistic[[1]], 9.576403, tolerance = 0.001)

    # with dropped factor level -------------------------------------------------

    # drop a factor level
    msleep_short <- dplyr::filter(.data = ggplot2::msleep, vore != "omni")

    # message
    testthat::expect_output(
      bartlett_message(
        data = msleep_short,
        x = vore,
        y = sleep_rem,
        k = 4
      ),
      "Note: Bartlett's test for homogeneity of variances for factor vore: p-value = 0.0188",
      fixed = TRUE
    )

    # stats results
    df <- bartlett_message(
      data = msleep_short,
      x = vore,
      y = sleep_rem,
      k = 4,
      output = "stats"
    )

    testthat::expect_equal(df$p.value[[1]], 0.01876283, tolerance = 0.001)
    testthat::expect_equal(df$statistic[[1]], 7.951755, tolerance = 0.001)
  }
)
