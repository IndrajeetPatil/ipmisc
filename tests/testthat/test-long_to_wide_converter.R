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
    testthat::expect_identical(
      names(df1),
      c("rowid", "Petal.Length", "Petal.Width", "Sepal.Length", "Sepal.Width")
    )

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
    testthat::expect_identical(
      names(df3),
      c("rowid", "HDHF", "HDLF", "LDHF", "LDLF")
    )

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
    testthat::expect_identical(
      names(df4),
      c("rowid", "carni", "herbi", "insecti", "omni")
    )
  }
)


testthat::test_that(
  desc = "long_to_wide_converter works - spread false",
  code = {
    df_summarizer <- function(data, x, y) {
      data %>%
        dplyr::group_by({{ x }}) %>%
        dplyr::group_modify(~ parameters::describe_distribution(
          x = (.) %>% dplyr::pull({{ y }}),
          centrality = "all"
        ))
    }

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

    # checking distributions
    set.seed(123)
    df1_dist <- df_summarizer(df1, condition, value)

    testthat::expect_equal(
      df1_dist,
      structure(list(
        condition = structure(1:4, .Label = c(
          "Petal.Length",
          "Petal.Width", "Sepal.Length", "Sepal.Width"
        ), class = "factor"),
        Median = c(4.35, 1.3, 5.8, 3), MAD = c(
          1.85325, 1.03782,
          1.03782, 0.44478
        ), Mean = c(
          3.758, 1.19933333333333, 5.84333333333333,
          3.05733333333333
        ), SD = c(
          1.76529823325947, 0.762237668960347,
          0.828066127977863, 0.435866284936698
        ), MAP = c(
          1.45562072336266,
          0.20791788856305, 5.74633431085044, 2.99706744868035
        ), IQR = c(
          3.525,
          1.5, 1.3, 0.525
        ), Min = c(1, 0.1, 4.3, 2), Max = c(
          6.9, 2.5,
          7.9, 4.4
        ), Skewness = c(
          -0.274884179751012, -0.10296674764898,
          0.314910956636973, 0.318965664713603
        ), Kurtosis = c(
          -1.40210341552175,
          -1.34060399661265, -0.552064041315639, 0.228249042468194
        ),
        n = c(150L, 150L, 150L, 150L), n_Missing = c(
          0L, 0L, 0L,
          0L
        )
      ), row.names = c(NA, -4L), groups = structure(list(
        condition = structure(1:4, .Label = c(
          "Petal.Length",
          "Petal.Width", "Sepal.Length", "Sepal.Width"
        ), class = "factor"),
        .rows = structure(list(1L, 2L, 3L, 4L), ptype = integer(0), class = c(
          "vctrs_list_of",
          "vctrs_vctr", "list"
        ))
      ), row.names = c(NA, 4L), class = c(
        "tbl_df",
        "tbl", "data.frame"
      ), .drop = TRUE), class = c(
        "grouped_df",
        "tbl_df", "tbl", "data.frame"
      ))
    )

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

    # checking distributions
    set.seed(123)
    df2_dist <- df_summarizer(df2, am, wt)

    testthat::expect_equal(dim(df2), c(32L, 3L))

    testthat::expect_equal(
      df2_dist,
      structure(list(
        am = structure(1:2, .Label = c("0", "1"), class = "factor"),
        Median = c(3.52, 2.32), MAD = c(0.452193, 0.681996), Mean = c(
          3.76889473684211,
          2.411
        ), SD = c(0.777400146838225, 0.616981631277085), MAP = c(
          3.47158064516129,
          2.33941935483871
        ), IQR = c(0.41, 0.9425), Min = c(
          2.465,
          1.513
        ), Max = c(5.424, 3.57), Skewness = c(
          1.15134151847953,
          0.269264150826873
        ), Kurtosis = c(1.05925609632379, -0.653832564906377),
        n = c(19L, 13L), n_Missing = c(0L, 0L)
      ), row.names = c(
        NA,
        -2L
      ), groups = structure(list(am = structure(1:2, .Label = c(
        "0",
        "1"
      ), class = "factor"), .rows = structure(list(1L, 2L),
        ptype = integer(0), class = c(
          "vctrs_list_of",
          "vctrs_vctr", "list"
        )
      )), row.names = 1:2, class = c(
        "tbl_df",
        "tbl", "data.frame"
      ), .drop = TRUE), class = c(
        "grouped_df",
        "tbl_df", "tbl", "data.frame"
      ))
    )

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

    set.seed(123)
    df3_dist <- df_summarizer(df3, condition, desire)

    testthat::expect_equal(dim(df3), c(352L, 3L))

    testthat::expect_equal(
      df3_dist,
      structure(list(condition = structure(1:4, .Label = c(
        "HDHF",
        "HDLF", "LDHF", "LDLF"
      ), class = "factor"), Median = c(
        8.5, 7.75,
        8, 6
      ), MAD = c(2.2239, 3.33585, 2.9652, 2.9652), Mean = c(
        7.82386363636364,
        6.67613636363636, 7.35227272727273, 5.65909090909091
      ), SD = c(
        2.48794769866775,
        3.13676729112354, 2.53952558121364, 2.67657511317507
      ), MAP = c(
        9.99022482893451,
        9.71652003910068, 9.82355816226784, 6.03128054740958
      ), IQR = c(
        4,
        5.375, 3.5, 4
      ), Min = c(0, 0, 0.5, 0), Max = c(10, 10, 10, 10), Skewness = c(
        -1.12561582513982, -0.702118050374947, -0.936849724698568,
        -0.109227155122267
      ), Kurtosis = c(
        0.481736193939875, -0.733925472593686,
        0.111662179973175, -0.694031535241595
      ), n = c(
        88L, 88L, 88L,
        88L
      ), n_Missing = c(0L, 0L, 0L, 0L)),
      row.names = c(NA, -4L),
      groups = structure(list(
        condition = structure(1:4, .Label = c(
          "HDHF", "HDLF", "LDHF",
          "LDLF"
        ), class = "factor"), .rows = structure(list(
          1L, 2L,
          3L, 4L
        ), ptype = integer(0), class = c(
          "vctrs_list_of",
          "vctrs_vctr", "list"
        ))
      ), row.names = c(NA, 4L), class = c(
        "tbl_df",
        "tbl", "data.frame"
      ), .drop = TRUE), class = c(
        "grouped_df",
        "tbl_df", "tbl", "data.frame"
      )
      )
    )

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

    set.seed(123)
    df4_dist <- df_summarizer(df4, vore, brainwt)

    testthat::expect_equal(dim(df4), c(51L, 3L))

    testthat::expect_equal(
      df4_dist,
      structure(list(
        vore = structure(1:4, .Label = c(
          "carni", "herbi",
          "insecti", "omni"
        ), class = "factor"), Median = c(
          0.0445, 0.012285,
          0.0012, 0.0066
        ), MAD = c(
          0.0400302, 0.017175921, 0.00140847,
          0.009577596
        ), Mean = c(
          0.0792555555555556, 0.6215975, 0.02155,
          0.145731176470588
        ), SD = c(
          0.102710711601944, 1.57200889436306,
          0.0348768547893872, 0.324596102408099
        ), MAP = c(
          0.0243139784946237,
          0.00598318670576735, 0.000565738025415445, 0.00530074291300098
        ), IQR = c(0.0985, 0.353625, 0.052725, 0.17695), Min = c(
          0.0108,
          4e-04, 0.00025, 0.00014
        ), Max = c(0.325, 5.712, 0.081, 1.32),
        Skewness = c(
          2.111434526443, 2.88988144394508, 1.79211175234383,
          3.34373554930475
        ), Kurtosis = c(
          4.45488569505209, 7.39712702894948,
          3.04397150215247, 12.0377893121251
        ), n = c(9L, 20L, 5L, 17L), n_Missing = c(0L, 0L, 0L, 0L)
      ), row.names = c(NA, -4L), groups = structure(list(
        vore = structure(1:4, .Label = c(
          "carni", "herbi", "insecti",
          "omni"
        ), class = "factor"), .rows = structure(list(
          1L, 2L,
          3L, 4L
        ), ptype = integer(0), class = c(
          "vctrs_list_of",
          "vctrs_vctr", "list"
        ))
      ), row.names = c(NA, 4L), class = c(
        "tbl_df",
        "tbl", "data.frame"
      ), .drop = TRUE), class = c(
        "grouped_df",
        "tbl_df", "tbl", "data.frame"
      ))
    )
  }
)


# with rowid - without NA ---------------------------------------------

testthat::test_that(
  desc = "with rowid - without NA",
  code = {
    df <-
      structure(list(score = c(90, 90, 72.5, 45), condition = structure(c(
        1L,
        2L, 2L, 1L
      ), .Label = c("4", "5"), class = "factor"), id = c(
        1L,
        2L, 1L, 2L
      )), row.names = c(NA, -4L), class = c(
        "tbl_df", "tbl",
        "data.frame"
      ))

    df1 <- dplyr::arrange(df, id)

    testthat::expect_equal(
      long_to_wide_converter(df1, condition, score),
      long_to_wide_converter(df, condition, score, id)
    )

    testthat::expect_equal(
      long_to_wide_converter(df1, "condition", score, paired = FALSE),
      long_to_wide_converter(df, condition, "score", id, paired = FALSE)
    )

    testthat::expect_equal(
      long_to_wide_converter(df1, condition, "score", paired = FALSE, spread = FALSE) %>%
        dplyr::arrange(rowid) %>%
        dplyr::select(-rowid),
      long_to_wide_converter(df, condition, score, "id", paired = FALSE, spread = FALSE) %>%
        dplyr::arrange(rowid) %>%
        dplyr::select(-rowid)
    )

    testthat::expect_equal(
      long_to_wide_converter(df1, condition, score, spread = FALSE) %>%
        dplyr::arrange(rowid),
      long_to_wide_converter(df, condition, score, "id", spread = FALSE) %>%
        dplyr::arrange(rowid)
    )
  }
)


# with rowid - with NA ---------------------------------------------

testthat::test_that(
  desc = "with rowid - without NA",
  code = {
    df <- bugs_long
    df1 <- dplyr::arrange(bugs_long, subject)

    testthat::expect_equal(
      long_to_wide_converter(df1, condition, desire) %>%
        dplyr::select(-rowid),
      long_to_wide_converter(df, condition, desire, subject) %>%
        dplyr::select(-rowid)
    )

    testthat::expect_equal(
      long_to_wide_converter(df1, "condition", desire, paired = FALSE) %>%
        dplyr::select(-rowid),
      long_to_wide_converter(df, condition, "desire", subject, paired = FALSE) %>%
        dplyr::select(-rowid)
    )

    testthat::expect_equal(
      long_to_wide_converter(df1, condition, desire, paired = FALSE, spread = FALSE) %>%
        dplyr::select(-rowid),
      long_to_wide_converter(df, condition, desire, "subject", paired = FALSE, spread = FALSE) %>%
        dplyr::select(-rowid)
    )

    testthat::expect_equal(
      long_to_wide_converter(df1, condition, "desire", spread = FALSE) %>%
        dplyr::select(-rowid),
      long_to_wide_converter(df, "condition", desire, subject, spread = FALSE) %>%
        dplyr::select(-rowid)
    )
  }
)
