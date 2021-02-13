# ipmisc 6.0.0

  - Removes `specify_decimal_p` alias for `format_num`.

  - Re-exports `%T>%` operator from `magrittr`.

  - In `long_to_wide_converter` function, the argument `spared` is ignored when
    `paired = FALSE`.

# ipmisc 5.0.2

  - Removes `crayon` from dependencies.

# ipmisc 5.0.1

  - Temporarily re-export the crayon function to salvage the `ggstatsplot`
    ecosystem.

# ipmisc 5.0.0

  - Retires vestigial `easystats_to_tidy_names` in favor of
    `insight::standardize_names`.

  - Removes color function re-exports from `crayon` as they are no longer used
    anywhere in the `ggstatsverse`.

  - Removes `outlier_df` function as it is only relevant for `ggstatsplot`.

  - Removes `set_cwd` function in favor of encouraging using `here::here()`.

  - Removes `signif_column` function as `ggstatsplot` no longer uses
    asterisks-based conventions anywhere to display *p*-values.

# ipmisc 4.1.0

  - `long_to_wide_converter` gains `subject.id` argument to avoid #13.

# ipmisc 4.0.0
 
  - `specify_decimal_p` no longer shows *p*-values less than 0.001 as `"<

0.001"`, but rather in scientific notation.

  - `p_value_formatter` function is removed as it is no longer used in the
    `ggstatsverse`.

# ipmisc 3.2.0

  - Hosts `easystats_to_tidy_names` function here now instead of `broomExtra`
    package. This helps make the `tidyBF` package lighter in terms of
    dependencies.

# ipmisc 3.1.0

  - Minor refactoring of `long_to_wide_converter`. It gains `spread` argument.
    When set to `FALSE`, the original dataframe will be returned with the `NA`'s
    removed.

  - New helper function `p_value_formatter`.

# ipmisc 3.0.0

  - Removes the unused `sort_xy` function. This removes `forcats` from
    `imports`.

  - Removes the unused `normality_message` and `shapiro_message` functions.

  - Removes the unneeded `movies_wide` and `movies_long` datasets.

# ipmisc 2.0.0

  - New function: `stats_type_switch`.

  - `signif_column` now explicitly returns a dataframe.

  - The following function are no longer re-exported from `broomExtra` to remove
    it from imports: `tidy`, `glance`, `augment`, and `easystats_to_tidy_names`.
    This gets rid of dependence on `broomExtra`.

  - `bartlett_message` and `normality_message` function lose `output` argument.
    They now always return a message.

  - Minimum R version needed is bumped to `R 3.6.0`.

# ipmisc 1.2.0

  - Re-exports `easystats_to_tidy_names` from `broomExtra`.

  - Fixes tests for the new release of `tibble`.

# ipmisc 1.1.0

  - Re-export `tibble::as_tibble` and color functions from `crayon`.

  - New function: `easystats_to_tidy_names`, `sort_xy`.

# ipmisc 1.0.0

  - Initial release of the package.

