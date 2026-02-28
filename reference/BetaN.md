# Estimate Standardized Regression Coefficients and the Corresponding Sampling Covariance Matrix Assuming Multivariate Normality

Estimate Standardized Regression Coefficients and the Corresponding
Sampling Covariance Matrix Assuming Multivariate Normality

## Usage

``` r
BetaN(object, alpha = c(0.05, 0.01, 0.001))
```

## Arguments

- object:

  Object of class `lm`.

- alpha:

  Numeric vector. Significance level \\\alpha\\.

## Value

Returns an object of class `betasandwich` which is a list with the
following elements:

- call:

  Function call.

- args:

  Function arguments.

- lm_process:

  Processed `lm` object.

- gamma_n:

  Asymptotic covariance matrix of the sample covariance matrix assuming
  multivariate normality.

- gamma_hc:

  Asymptotic covariance matrix HC correction.

- gamma:

  Asymptotic covariance matrix of the sample covariance matrix.

- acov:

  Asymptotic covariance matrix of the standardized slopes.

- vcov:

  Sampling covariance matrix of the standardized slopes.

- est:

  Vector of standardized slopes.

## Details

Note that while the calculation in `BetaN()` is different from
[`betaDelta::BetaDelta()`](https://github.com/jeksterslab/betaDelta/reference/BetaDelta.html)
with `type = "mvn"`, the results are numerically equivalent. `BetaN()`
assumes multivariate normality.
[`BetaHC()`](https://github.com/jeksterslab/betaSandwich/reference/BetaHC.md)
is recommended in most situations.

## References

Dudgeon, P. (2017). Some improvements in confidence intervals for
standardized regression coefficients. *Psychometrika*, *82*(4), 928–951.
[doi:10.1007/s11336-017-9563-z](https://doi.org/10.1007/s11336-017-9563-z)

Pesigan, I. J. A., Sun, R. W., & Cheung, S. F. (2023). betaDelta and
betaSandwich: Confidence intervals for standardized regression
coefficients in R. *Multivariate Behavioral Research*.
[doi:10.1080/00273171.2023.2201277](https://doi.org/10.1080/00273171.2023.2201277)

## See also

Other Beta Sandwich Functions:
[`BetaADF()`](https://github.com/jeksterslab/betaSandwich/reference/BetaADF.md),
[`BetaHC()`](https://github.com/jeksterslab/betaSandwich/reference/BetaHC.md),
[`DiffBetaSandwich()`](https://github.com/jeksterslab/betaSandwich/reference/DiffBetaSandwich.md),
[`RSqBetaSandwich()`](https://github.com/jeksterslab/betaSandwich/reference/RSqBetaSandwich.md)

## Author

Ivan Jacob Agaloos Pesigan

## Examples

``` r
object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
std <- BetaN(object)
# Methods -------------------------------------------------------
print(std)
#> Call:
#> BetaN(object = object)
#> 
#> Standardized regression slopes with MVN standard errors:
#>            est     se      t df     p   0.05%   0.5%   2.5%  97.5%  99.5%
#> NARTIC  0.4951 0.0759 6.5272 42 0.000  0.2268 0.2905 0.3421 0.6482 0.6998
#> PCTGRT  0.3915 0.0770 5.0824 42 0.000  0.1190 0.1837 0.2360 0.5469 0.5993
#> PCTSUPP 0.2632 0.0747 3.5224 42 0.001 -0.0011 0.0616 0.1124 0.4141 0.4649
#>         99.95%
#> NARTIC  0.7635
#> PCTGRT  0.6640
#> PCTSUPP 0.5276
summary(std)
#> Call:
#> BetaN(object = object)
#> 
#> Standardized regression slopes with MVN standard errors:
#>            est     se      t df     p   0.05%   0.5%   2.5%  97.5%  99.5%
#> NARTIC  0.4951 0.0759 6.5272 42 0.000  0.2268 0.2905 0.3421 0.6482 0.6998
#> PCTGRT  0.3915 0.0770 5.0824 42 0.000  0.1190 0.1837 0.2360 0.5469 0.5993
#> PCTSUPP 0.2632 0.0747 3.5224 42 0.001 -0.0011 0.0616 0.1124 0.4141 0.4649
#>         99.95%
#> NARTIC  0.7635
#> PCTGRT  0.6640
#> PCTSUPP 0.5276
coef(std)
#>    NARTIC    PCTGRT   PCTSUPP 
#> 0.4951451 0.3914887 0.2632477 
vcov(std)
#>               NARTIC       PCTGRT      PCTSUPP
#> NARTIC   0.005754524 -0.003360334 -0.002166127
#> PCTGRT  -0.003360334  0.005933462 -0.001769723
#> PCTSUPP -0.002166127 -0.001769723  0.005585256
confint(std, level = 0.95)
#>             2.5 %    97.5 %
#> NARTIC  0.3420563 0.6482339
#> PCTGRT  0.2360380 0.5469395
#> PCTSUPP 0.1124272 0.4140682
```
