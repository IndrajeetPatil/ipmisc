Tests and Coverage
================
16 January, 2020 22:20:39

  - [Coverage](#coverage)
  - [Unit Tests](#unit-tests)

This output is created by
[covrpage](https://github.com/metrumresearchgroup/covrpage).

## Coverage

Coverage summary is created using the
[covr](https://github.com/r-lib/covr) package.

| Object                                                         | Coverage (%) |
| :------------------------------------------------------------- | :----------: |
| ipmisc                                                         |    97.22     |
| [R/set\_cwd.R](../R/set_cwd.R)                                 |     0.00     |
| [R/long\_to\_wide\_converter.R](../R/long_to_wide_converter.R) |    100.00    |
| [R/outlier\_df.R](../R/outlier_df.R)                           |    100.00    |
| [R/signif\_column.R](../R/signif_column.R)                     |    100.00    |
| [R/specify\_decimal\_p.R](../R/specify_decimal_p.R)            |    100.00    |

<br>

## Unit Tests

Unit Test summary is created using the
[testthat](https://github.com/r-lib/testthat) package.

| file                                                                       | n | time | error | failed | skipped | warning |
| :------------------------------------------------------------------------- | -: | ---: | ----: | -----: | ------: | ------: |
| [test-long\_to\_wide\_converter.R](testthat/test-long_to_wide_converter.R) | 4 | 0.06 |     0 |      0 |       0 |       0 |
| [test-outlier\_df.R](testthat/test-outlier_df.R)                           | 4 | 0.02 |     0 |      0 |       0 |       0 |
| [test-signif\_column.R](testthat/test-signif_column.R)                     | 9 | 0.12 |     0 |      0 |       0 |       0 |
| [test-specify\_decimal\_p.R](testthat/test-specify_decimal_p.R)            | 9 | 0.02 |     0 |      0 |       0 |       0 |

<details closed>

<summary> Show Detailed Test Results </summary>

| file                                                                           | context                   |              test               | status | n | time |
| :----------------------------------------------------------------------------- | :------------------------ | :-----------------------------: | :----- | -: | ---: |
| [test-long\_to\_wide\_converter.R](testthat/test-long_to_wide_converter.R#L26) | long\_to\_wide\_converter | long\_to\_wide\_converter works | PASS   | 4 | 0.06 |
| [test-outlier\_df.R](testthat/test-outlier_df.R#L23)                           | outlier\_df               |  outlier\_df works as expected  | PASS   | 4 | 0.02 |
| [test-signif\_column.R](testthat/test-signif_column.R#L45)                     | signif column             |      signif\_column works       | PASS   | 9 | 0.12 |
| [test-specify\_decimal\_p.R](testthat/test-specify_decimal_p.R#L26)            | Specify decimals          |    specify\_decimal\_p works    | PASS   | 9 | 0.02 |

</details>

<details>

<summary> Session Info </summary>

| Field    | Value                            |
| :------- | :------------------------------- |
| Version  | R version 3.6.2 (2019-12-12)     |
| Platform | x86\_64-w64-mingw32/x64 (64-bit) |
| Running  | Windows 10 x64 (build 16299)     |
| Language | English\_United States           |
| Timezone | Europe/Berlin                    |

| Package  | Version |
| :------- | :------ |
| testthat | 2.3.1   |
| covr     | 3.4.0   |
| covrpage | 0.0.70  |

</details>

<!--- Final Status : pass --->
