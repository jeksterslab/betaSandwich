betaSandwich
================
Ivan Jacob Agaloos Pesigan
2024-04-11

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![CRAN
Status](https://www.r-pkg.org/badges/version/betaSandwich)](https://cran.r-project.org/package=betaSandwich)
[![R-Universe
Status](https://jeksterslab.r-universe.dev/badges/betaSandwich)](https://jeksterslab.r-universe.dev)
[![DOI](https://zenodo.org/badge/DOI/10.1080/00273171.2023.2201277.svg)](https://doi.org/10.1080/00273171.2023.2201277)
[![Make
Project](https://github.com/jeksterslab/betaSandwich/actions/workflows/make.yml/badge.svg)](https://github.com/jeksterslab/betaSandwich/actions/workflows/make.yml)
[![R-CMD-check](https://github.com/jeksterslab/betaSandwich/actions/workflows/check-full.yml/badge.svg)](https://github.com/jeksterslab/betaSandwich/actions/workflows/check-full.yml)
[![R Package Test
Coverage](https://github.com/jeksterslab/betaSandwich/actions/workflows/test-coverage.yml/badge.svg)](https://github.com/jeksterslab/betaSandwich/actions/workflows/test-coverage.yml)
[![Lint R
Package](https://github.com/jeksterslab/betaSandwich/actions/workflows/lint.yml/badge.svg)](https://github.com/jeksterslab/betaSandwich/actions/workflows/lint.yml)
[![Package Website (GitHub
Pages)](https://github.com/jeksterslab/betaSandwich/actions/workflows/pkgdown-gh-pages.yml/badge.svg)](https://github.com/jeksterslab/betaSandwich/actions/workflows/pkgdown-gh-pages.yml)
[![Compile
LaTeX](https://github.com/jeksterslab/betaSandwich/actions/workflows/latex.yml/badge.svg)](https://github.com/jeksterslab/betaSandwich/actions/workflows/latex.yml)
[![Shell
Check](https://github.com/jeksterslab/betaSandwich/actions/workflows/shellcheck.yml/badge.svg)](https://github.com/jeksterslab/betaSandwich/actions/workflows/shellcheck.yml)
[![pages-build-deployment](https://github.com/jeksterslab/betaSandwich/actions/workflows/pages/pages-build-deployment/badge.svg)](https://github.com/jeksterslab/betaSandwich/actions/workflows/pages/pages-build-deployment)
[![codecov](https://codecov.io/gh/jeksterslab/betaSandwich/branch/main/graph/badge.svg?token=KVLUET3DJ6)](https://codecov.io/gh/jeksterslab/betaSandwich)
<!-- badges: end -->

## Description

Generates robust confidence intervals for standardized regression
coefficients using heteroskedasticity-consistent standard errors for
models fitted by `lm()` as described in Dudgeon (2017:
<http://doi.org/10.1007/s11336-017-9563-z>). The package can also be
used to generate confidence intervals for R-squared, adjusted R-squared,
and differences of standardized regression coefficients. A description
of the package and code examples are presented in Pesigan, Sun, and
Cheung (2023: <https://doi.org/10.1080/00273171.2023.2201277>).

## Installation

You can install the CRAN release of `betaSandwich` with:

``` r
install.packages("betaSandwich")
```

You can install the development version of `betaSandwich` from
[GitHub](https://github.com/jeksterslab/betaSandwich) with:

``` r
if (!require("remotes")) install.packages("remotes")
remotes::install_github("jeksterslab/betaSandwich")
```

## Example

In this example, a multiple regression model is fitted using program
quality ratings (`QUALITY`) as the regressand/outcome variable and
number of published articles attributed to the program faculty members
(`NARTIC`), percent of faculty members holding research grants
(`PCTGRT`), and percentage of program graduates who received support
(`PCTSUPP`) as regressor/predictor variables using a data set from 1982
ratings of 46 doctoral programs in psychology in the USA (National
Research Council, 1982). Robust confidence intervals for the
standardized regression coefficients are generated using the `BetaHC()`
function from the `betaSandwich` package following Dudgeon (2017).

``` r
library(betaSandwich)
```

``` r
df <- betaSandwich::nas1982
```

### Fit the regression model using the `lm()` function.

``` r
object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = df)
```

### Estimate the standardized regression slopes and the corresponding robust sampling covariance matrix.

``` r
BetaHC(object, type = "hc3", alpha = 0.05)
#> Call:
#> BetaHC(object = object, type = "hc3", alpha = 0.05)
#> 
#> Standardized regression slopes with HC3 standard errors:
#>            est     se      t df      p   2.5%  97.5%
#> NARTIC  0.4951 0.0786 6.3025 42 0.0000 0.3366 0.6537
#> PCTGRT  0.3915 0.0818 4.7831 42 0.0000 0.2263 0.5567
#> PCTSUPP 0.2632 0.0855 3.0786 42 0.0037 0.0907 0.4358
```

## Other Features

The package can also be used to generate confidence intervals for
R-squared, adjusted R-squared, and differences of standardized
regression coefficients.

## Documentation

See [GitHub
Pages](https://jeksterslab.github.io/betaSandwich/index.html) for
package documentation.

## Citation

To cite `betaSandwich` in publications, please cite Pesigan et al.
(2023).

## References

<div id="refs" class="references csl-bib-body hanging-indent"
line-spacing="2">

<div id="ref-Dudgeon-2017" class="csl-entry">

Dudgeon, P. (2017). Some improvements in confidence intervals for
standardized regression coefficients. *Psychometrika*, *82*(4), 928–951.
<https://doi.org/10.1007/s11336-017-9563-z>

</div>

<div id="ref-NationalResearchCouncil-1982" class="csl-entry">

National Research Council. (1982). *An assessment of research-doctorate
programs in the United States: Social and behavioral sciences*. National
Academies Press. <https://doi.org/10.17226/9781>

</div>

<div id="ref-Pesigan-Sun-Cheung-2023" class="csl-entry">

Pesigan, I. J. A., Sun, R. W., & Cheung, S. F. (2023).
<span class="nocase">betaDelta</span> and
<span class="nocase">betaSandwich</span>: Confidence intervals for
standardized regression coefficients in R. *Multivariate Behavioral
Research*, 1–4. <https://doi.org/10.1080/00273171.2023.2201277>

</div>

</div>
