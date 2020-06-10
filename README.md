
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Power Analysis for Longitudinal Genome-Wide Association Studies

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![GitHub
tag](https://img.shields.io/github/tag/mcanouil/palgwas.svg?label=latest%20tag&include_prereleases)](https://github.com/mcanouil/palgwas)
[![Travis-CI Build
Status](https://travis-ci.org/mcanouil/palgwas.svg?branch=master)](https://travis-ci.org/mcanouil/palgwas)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/mcanouil/palgwas?branch=master&svg=true)](https://ci.appveyor.com/project/mcanouil/palgwas)
[![Coverage Status
(codecov)](https://codecov.io/gh/mcanouil/palgwas/branch/master/graph/badge.svg)](https://codecov.io/gh/mcanouil/palgwas)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version-ago/palgwas)](https://cran.r-project.org/package=palgwas)
[![cran
checks\_worst](https://cranchecks.info/badges/worst/palgwas)](https://cran.r-project.org/web/checks/check_results_palgwas.html)
[![CRAN\_Download\_total](https://cranlogs.r-pkg.org/badges/palgwas)](https://cran.r-project.org/package=palgwas)
<!-- badges: end --> Shiny App to explore statistical power for
longitudinal Genome-Wide Association Studies using various parameters
and two approach, namely, Generalised Estimating Equations and Linear
Mixed effect Model.

## Installation

``` r
# Install palgwas from CRAN:
install.packages("palgwas")

# Or the the development version from GitHub:
# install.packages("remotes")
remotes::install_github("mcanouil/palgwas")
```

``` r
library("palgwas")
go_palgwas()
```

## Getting help

If you encounter a clear bug, please file a minimal reproducible example
on [github](https://github.com/mcanouil/palgwas/issues).  
For questions and other discussion, please contact the package
maintainer.

-----

Please note that the palgwas project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
