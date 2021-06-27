
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `ipmisc`: Miscellaneous Functions for Data Cleaning and Analysis

[![CRAN\_Release\_Badge](https://www.r-pkg.org/badges/version-ago/ipmisc)](https://CRAN.R-project.org/package=ipmisc)
[![Total downloads
badge](https://cranlogs.r-pkg.org/badges/grand-total/ipmisc?color=blue)](https://CRAN.R-project.org/package=ipmisc)
[![R build
status](https://github.com/IndrajeetPatil/ipmisc/workflows/R-CMD-check/badge.svg)](https://github.com/IndrajeetPatil/ipmisc)
[![pkgdown](https://github.com/IndrajeetPatil/ipmisc/workflows/pkgdown/badge.svg)](https://github.com/IndrajeetPatil/ipmisc/actions)
[![Licence](https://img.shields.io/badge/licence-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![Codecov test
coverage](https://codecov.io/gh/IndrajeetPatil/ipmisc/branch/master/graph/badge.svg)](https://codecov.io/gh/IndrajeetPatil/ipmisc?branch=master)

# Introduction

The `ipmisc` provides functions for data cleaning and formatting. These
functions form data cleaning backend for the following packages:

-   `ggstatsplot`: <https://indrajeetpatil.github.io/ggstatsplot/>
-   `statsExpressions`:
    <https://indrajeetpatil.github.io/statsExpressions/>
-   `pairwiseComparisons`:
    <https://indrajeetpatil.github.io/pairwiseComparisons/>

**Note**: The `ipmisc` functions are not expected to have much utility
outside of these packages. So, if you wish to use them, do so at your
own risk. 😉

# Installation

| Type        | Source | Command                                            |
|-------------|--------|----------------------------------------------------|
| Release     | CRAN   | `install.packages("ipmisc")`                       |
| Development | GitHub | `remotes::install_github("IndrajeetPatil/ipmisc")` |

# Functions

Function to convert a tidy data to wide data:

``` r
library(ipmisc)

long_to_wide_converter(bugs_long, condition, desire, paired = TRUE)
#> # A tibble: 88 x 5
#>    rowid  HDHF  HDLF  LDHF  LDLF
#>    <int> <dbl> <dbl> <dbl> <dbl>
#>  1     1  10     9     6     6  
#>  2     3  10    10    10     5  
#>  3     4   9     6     9     6  
#>  4     5   8.5   5.5   6.5   3  
#>  5     6   3     7.5   0.5   2  
#>  6     7  10    10    10    10  
#>  7     8  10     9    10    10  
#>  8     9  10     6     9.5   9.5
#>  9    11   0     0     2.5   0  
#> 10    12  10     8.5   7.5   9.5
#> # … with 78 more rows
```

To see all available functionality, see the documentation provided here:
<https://indrajeetpatil.github.io/ipmisc/reference/index.html>
