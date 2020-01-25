## Test environments
* local OS X install, R 3.6.2
* ubuntu 14.04 (on travis-ci), R 3.6.2
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

  - This is a new release.
  - I would prefer to retain `LICENSE` file in the `DESCRIPTION` so that this
    file can be retained on `GitHub` and users can easily access it. This is
    also the case with all the other packages I currently have on `CRAN`, and so
    this also allows me to be consistent across my own packages.
  - All functions now have a `value` section in their documentation.
  - Note that all functions that print information to users console have a
    `output = "message"` argument. Users can suppress this information, by
    setting this argument to other values. The only exception to this is
    `set_cwd`, but that behavior comes from `rstudioapi` package and beyond
    `ipmisc` control. 
