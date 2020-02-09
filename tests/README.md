Tests and Coverage
================
09 February, 2020 10:47:04

  - [Coverage](#coverage)
  - [Unit Tests](#unit-tests)

This output is created by
[covrpage](https://github.com/metrumresearchgroup/covrpage).

## Coverage

Coverage summary is created using the
[covr](https://github.com/r-lib/covr) package.

| Object                                                           | Coverage (%) |
| :--------------------------------------------------------------- | :----------: |
| ipmisc                                                           |     98.6     |
| [R/set\_cwd.R](../R/set_cwd.R)                                   |     0.0      |
| [R/easystats\_to\_tidy\_names.R](../R/easystats_to_tidy_names.R) |    100.0     |
| [R/helpers\_messages.R](../R/helpers_messages.R)                 |    100.0     |
| [R/long\_to\_wide\_converter.R](../R/long_to_wide_converter.R)   |    100.0     |
| [R/outlier\_df.R](../R/outlier_df.R)                             |    100.0     |
| [R/signif\_column.R](../R/signif_column.R)                       |    100.0     |
| [R/sort\_xy.R](../R/sort_xy.R)                                   |    100.0     |
| [R/specify\_decimal\_p.R](../R/specify_decimal_p.R)              |    100.0     |

<br>

## Unit Tests

Unit Test summary is created using the
[testthat](https://github.com/r-lib/testthat) package.

| file                                                                         |  n | time | error | failed | skipped | warning |
| :--------------------------------------------------------------------------- | -: | ---: | ----: | -----: | ------: | ------: |
| [test-easystats\_to\_tidy\_names.R](testthat/test-easystats_to_tidy_names.R) |  3 | 0.35 |     0 |      0 |       0 |       0 |
| [test-helper\_messages.R](testthat/test-helper_messages.R)                   | 12 | 0.04 |     0 |      0 |       0 |       0 |
| [test-long\_to\_wide\_converter.R](testthat/test-long_to_wide_converter.R)   |  4 | 0.06 |     0 |      0 |       0 |       0 |
| [test-outlier\_df.R](testthat/test-outlier_df.R)                             |  4 | 0.03 |     0 |      0 |       0 |       0 |
| [test-signif\_column.R](testthat/test-signif_column.R)                       |  9 | 0.02 |     0 |      0 |       0 |       0 |
| [test-sort\_xy.R](testthat/test-sort_xy.R)                                   |  8 | 0.01 |     0 |      0 |       0 |       0 |
| [test-specify\_decimal\_p.R](testthat/test-specify_decimal_p.R)              |  9 | 0.02 |     0 |      0 |       0 |       0 |

<details closed>

<summary> Show Detailed Test Results </summary>

| file                                                                             | context                    |                     test                     | status | n | time |
| :------------------------------------------------------------------------------- | :------------------------- | :------------------------------------------: | :----- | -: | ---: |
| [test-easystats\_to\_tidy\_names.R](testthat/test-easystats_to_tidy_names.R#L12) | easystats\_to\_tidy\_names | easystats\_to\_tidy\_names works as expected | PASS   | 3 | 0.35 |
| [test-helper\_messages.R](testthat/test-helper_messages.R#L8_L12)                | helper\_messages           |        normality\_message is working         | PASS   | 3 | 0.01 |
| [test-helper\_messages.R](testthat/test-helper_messages.R#L37_L46)               | helper\_messages           |         bartlett\_message is working         | PASS   | 9 | 0.03 |
| [test-long\_to\_wide\_converter.R](testthat/test-long_to_wide_converter.R#L26)   | long\_to\_wide\_converter  |       long\_to\_wide\_converter works        | PASS   | 4 | 0.06 |
| [test-outlier\_df.R](testthat/test-outlier_df.R#L19)                             | outlier\_df                |        outlier\_df works as expected         | PASS   | 4 | 0.03 |
| [test-signif\_column.R](testthat/test-signif_column.R#L45)                       | signif column              |             signif\_column works             | PASS   | 9 | 0.02 |
| [test-sort\_xy.R](testthat/test-sort_xy.R#L14_L17)                               | sort\_xy                   |          sort\_xy works as expected          | PASS   | 8 | 0.01 |
| [test-specify\_decimal\_p.R](testthat/test-specify_decimal_p.R#L26)              | Specify decimals           |          specify\_decimal\_p works           | PASS   | 9 | 0.02 |

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
