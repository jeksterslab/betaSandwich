# Estimate Standardized Regression Coefficients and the Corresponding Sampling Covariance Matrix Using the Asymptotic Distribution-Free Approach

Estimate Standardized Regression Coefficients and the Corresponding
Sampling Covariance Matrix Using the Asymptotic Distribution-Free
Approach

## Usage

``` r
BetaADF(object, alpha = c(0.05, 0.01, 0.001))
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

Note that while the calculation in `BetaADF()` is different from
[`betaDelta::BetaDelta()`](https://github.com/jeksterslab/betaDelta/reference/BetaDelta.html)
with `type = "adf"`, the results are numerically equivalent. `BetaADF()`
is appropriate when sample sizes are moderate to large (`n > 250`).
[`BetaHC()`](https://github.com/jeksterslab/betaSandwich/reference/BetaHC.md)
is recommended in most situations.

## References

Browne, M. W. (1984). Asymptotically distribution-free methods for the
analysis of covariance structures. *British Journal of Mathematical and
Statistical Psychology*, *37*(1), 62–83.
[doi:10.1111/j.2044-8317.1984.tb00789.x](https://doi.org/10.1111/j.2044-8317.1984.tb00789.x)

Dudgeon, P. (2017). Some improvements in confidence intervals for
standardized regression coefficients. *Psychometrika*, *82*(4), 928–951.
[doi:10.1007/s11336-017-9563-z](https://doi.org/10.1007/s11336-017-9563-z)

Pesigan, I. J. A., Sun, R. W., & Cheung, S. F. (2023). betaDelta and
betaSandwich: Confidence intervals for standardized regression
coefficients in R. *Multivariate Behavioral Research*.
[doi:10.1080/00273171.2023.2201277](https://doi.org/10.1080/00273171.2023.2201277)

## See also

Other Beta Sandwich Functions:
[`BetaHC()`](https://github.com/jeksterslab/betaSandwich/reference/BetaHC.md),
[`BetaN()`](https://github.com/jeksterslab/betaSandwich/reference/BetaN.md),
[`DiffBetaSandwich()`](https://github.com/jeksterslab/betaSandwich/reference/DiffBetaSandwich.md),
[`RSqBetaSandwich()`](https://github.com/jeksterslab/betaSandwich/reference/RSqBetaSandwich.md)

## Author

Ivan Jacob Agaloos Pesigan

## Examples

``` r
object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
std <- BetaADF(object)
# Methods -------------------------------------------------------
print(std)
#> Call:
#> BetaADF(object = object)
#> 
#> Standardized regression slopes with MVN standard errors:
#>            est     se      t df      p   0.05%   0.5%   2.5%  97.5%  99.5%
#> NARTIC  0.4951 0.0674 7.3490 42 0.0000  0.2568 0.3134 0.3592 0.6311 0.6769
#> PCTGRT  0.3915 0.0710 5.5164 42 0.0000  0.1404 0.2000 0.2483 0.5347 0.5830
#> PCTSUPP 0.2632 0.0769 3.4231 42 0.0014 -0.0088 0.0558 0.1081 0.4184 0.4707
#>         99.95%
#> NARTIC  0.7335
#> PCTGRT  0.6426
#> PCTSUPP 0.5353
summary(std)
#> Call:
#> BetaADF(object = object)
#> 
#> Standardized regression slopes with MVN standard errors:
#>            est     se      t df      p   0.05%   0.5%   2.5%  97.5%  99.5%
#> NARTIC  0.4951 0.0674 7.3490 42 0.0000  0.2568 0.3134 0.3592 0.6311 0.6769
#> PCTGRT  0.3915 0.0710 5.5164 42 0.0000  0.1404 0.2000 0.2483 0.5347 0.5830
#> PCTSUPP 0.2632 0.0769 3.4231 42 0.0014 -0.0088 0.0558 0.1081 0.4184 0.4707
#>         99.95%
#> NARTIC  0.7335
#> PCTGRT  0.6426
#> PCTSUPP 0.5353
coef(std)
#>    NARTIC    PCTGRT   PCTSUPP 
#> 0.4951451 0.3914887 0.2632477 
vcov(std)
#>               NARTIC       PCTGRT      PCTSUPP
#> NARTIC   0.004539472 -0.002552698 -0.001742698
#> PCTGRT  -0.002552698  0.005036538 -0.001906216
#> PCTSUPP -0.001742698 -0.001906216  0.005914088
confint(std, level = 0.95)
#>             2.5 %    97.5 %
#> NARTIC  0.3591757 0.6311146
#> PCTGRT  0.2482683 0.5347091
#> PCTSUPP 0.1080509 0.4184444
```
