
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Power Analysis for Longitudinal Genome-Wide Association Studies

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![GitHub
tag](https://img.shields.io/github/tag/mcanouil/palgwas.svg?label=latest%20tag&include_prereleases)](https://github.com/mcanouil/palgwas)
[![R-CMD-check](https://github.com/mcanouil/palgwas/actions/workflows/check-pak.yaml/badge.svg?branch=main)](https://github.com/mcanouil/palgwas/actions/workflows/check-pak.yaml)
<!-- badges: end -->

Shiny App to explore statistical power for longitudinal Genome-Wide
Association Studies using various parameters and two approaches, namely,
Generalised Estimating Equations and Linear Mixed effect Model.

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

------------------------------------------------------------------------

## Getting help

If you encounter a clear bug, please file a minimal reproducible example
on [GitHub](https://github.com/mcanouil/palgwas/issues).  
For questions and other discussion, please open a discussion on
[GitHub](https://github.com/mcanouil/palgwas/discussions).

## Code of Conduct

Please note that the `eggla` project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).  
By contributing to this project, you agree to abide by its terms.
