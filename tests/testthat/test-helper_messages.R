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
  }
)
