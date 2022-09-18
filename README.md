betaSandwich
================
Ivan Jacob Agaloos Pesigan
2022-09-18

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![R-CMD-check](https://github.com/jeksterslab/betaSandwich/workflows/R-CMD-check/badge.svg)](https://github.com/jeksterslab/betaSandwich/actions)
[![test-coverage](https://github.com/jeksterslab/betaSandwich/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/jeksterslab/betaSandwich/actions/workflows/test-coverage.yaml)
[![codecov](https://codecov.io/gh/jeksterslab/betaSandwich/branch/main/graph/badge.svg)](https://codecov.io/gh/jeksterslab/betaSandwich)
<!-- badges: end -->

## Description

Generates robust confidence intervals for standardized regression
coefficients using heteroskedasticity-consistent standard errors for
models fitted by `lm()` as described in Dudgeon (2017:
<http://doi.org/10.1007/s11336-017-9563-z>).

## Installation

You can install the released version of `betaSandwich` from
[GitHub](https://github.com/jeksterslab/betaSandwich) with:

``` r
install.packages("remotes")
remotes::install_github("jeksterslab/betaSandwich")
```

## Documentation

See [GitHub
Pages](https://jeksterslab.github.io/betaSandwich/index.html) for
package documentation.

## Example

In this example, a multiple regression model is fitted using the overall
rating (`rating`) as the regressand/outcome variable and the rest of the
variables as regressor/predictor variables using the
`datasets::attitude` data set (Chatterjee & Price, 1977). Robust
confidence intervals for the standardized regression coefficients are
generated using the `BetaHC()` function from the `betaSandwich` package
following Dudgeon (2017).

``` r
library(betaSandwich)
```

### Fit the regression model using the `lm()` function.

``` r
object <- lm(rating ~ ., data = attitude)
```

### Estimate the standardized regression slopes and the corresponding robust sampling covariance matrix.

``` r
BetaHC(object, type = "hc3")
#> Call:
#> BetaHC(object = object, type = "hc3")
#> 
#> Standardized regression slopes with HC3 standard errors:
#>                 est     se       t         p    0.05%    0.5%     2.5%  97.5%
#> complaints  0.67073 0.1560  4.2983 0.0002677  0.08281  0.2327  0.34792 0.9935
#> privileges -0.07343 0.1594 -0.4608 0.6493035 -0.67385 -0.5208 -0.40310 0.2562
#> learning    0.30887 0.1954  1.5806 0.1276145 -0.42735 -0.2397 -0.09536 0.7131
#> raises      0.06981 0.1843  0.3787 0.7083821 -0.62473 -0.4477 -0.31153 0.4512
#> critical    0.03120 0.1772  0.1761 0.8617538 -0.63630 -0.4662 -0.33530 0.3977
#> advance    -0.18346 0.1802 -1.0183 0.3191381 -0.86228 -0.6893 -0.55618 0.1892
#>             99.5% 99.95%
#> complaints 1.1088 1.2586
#> privileges 0.3740 0.5270
#> learning   0.8574 1.0451
#> raises     0.5873 0.7644
#> critical   0.5286 0.6987
#> advance    0.3223 0.4954
```

### References

Dudgeon, P. (2017). Some improvements in confidence intervals for
standardized regression coefficients. *Psychometrika*, 82 (4), 928–951.
<https://doi.org/10.1007/s11336-017-9563-z>

Chatterjee, S. and Price, B. (1977). *Regression Analysis by Example*.
New York: Wiley. (Section 3.7, p.68ff of 2nd ed.(1991).)
