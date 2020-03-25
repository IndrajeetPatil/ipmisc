
# switch for stats type works ------------------------------------------

testthat::test_that(
  desc = "switch for stats type works",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    testthat::expect_identical(stats_type_switch("p"), "parametric")
    testthat::expect_identical(stats_type_switch("pearson"), "parametric")
    testthat::expect_identical(stats_type_switch("non-parametric"), "nonparametric")
    testthat::expect_identical(stats_type_switch("np"), "nonparametric")
    testthat::expect_identical(stats_type_switch("r"), "robust")
    testthat::expect_identical(stats_type_switch("bf"), "bayes")
    testthat::expect_identical(stats_type_switch("xxx"), "parametric")
  }
)
