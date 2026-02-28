# Estimate Standardized Regression Coefficients and the Corresponding Robust Sampling Covariance Matrix Using the Heteroskedasticity Consistent Approach

Estimate Standardized Regression Coefficients and the Corresponding
Robust Sampling Covariance Matrix Using the Heteroskedasticity
Consistent Approach

## Usage

``` r
BetaHC(
  object,
  type = "hc3",
  alpha = c(0.05, 0.01, 0.001),
  g1 = 1,
  g2 = 1.5,
  k = 0.7
)
```

## Arguments

- object:

  Object of class `lm`.

- type:

  Character string. Correction type. Possible values are `"hc0"`,
  `"hc1"`, `"hc2"`, `"hc3"`, `"hc4"`, `"hc4m"`, and `"hc5"`.

- alpha:

  Numeric vector. Significance level \\\alpha\\.

- g1:

  Numeric. `g1` value for `type = "hc4m"`.

- g2:

  Numeric. `g2` value for `type = "hc4m"`.

- k:

  Numeric. Constant `k` for `type = "hc5"` \\0 \leq k \leq 1\\.

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
[`BetaN()`](https://github.com/jeksterslab/betaSandwich/reference/BetaN.md),
[`DiffBetaSandwich()`](https://github.com/jeksterslab/betaSandwich/reference/DiffBetaSandwich.md),
[`RSqBetaSandwich()`](https://github.com/jeksterslab/betaSandwich/reference/RSqBetaSandwich.md)

## Author

Ivan Jacob Agaloos Pesigan

## Examples

``` r
object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
std <- BetaHC(object)
# Methods -------------------------------------------------------
print(std)
#> Call:
#> BetaHC(object = object)
#> 
#> Standardized regression slopes with HC3 standard errors:
#>            est     se      t df      p   0.05%   0.5%   2.5%  97.5%  99.5%
#> NARTIC  0.4951 0.0786 6.3025 42 0.0000  0.2172 0.2832 0.3366 0.6537 0.7071
#> PCTGRT  0.3915 0.0818 4.7831 42 0.0000  0.1019 0.1707 0.2263 0.5567 0.6123
#> PCTSUPP 0.2632 0.0855 3.0786 42 0.0037 -0.0393 0.0325 0.0907 0.4358 0.4940
#>         99.95%
#> NARTIC  0.7731
#> PCTGRT  0.6810
#> PCTSUPP 0.5658
summary(std)
#> Call:
#> BetaHC(object = object)
#> 
#> Standardized regression slopes with HC3 standard errors:
#>            est     se      t df      p   0.05%   0.5%   2.5%  97.5%  99.5%
#> NARTIC  0.4951 0.0786 6.3025 42 0.0000  0.2172 0.2832 0.3366 0.6537 0.7071
#> PCTGRT  0.3915 0.0818 4.7831 42 0.0000  0.1019 0.1707 0.2263 0.5567 0.6123
#> PCTSUPP 0.2632 0.0855 3.0786 42 0.0037 -0.0393 0.0325 0.0907 0.4358 0.4940
#>         99.95%
#> NARTIC  0.7731
#> PCTGRT  0.6810
#> PCTSUPP 0.5658
coef(std)
#>    NARTIC    PCTGRT   PCTSUPP 
#> 0.4951451 0.3914887 0.2632477 
vcov(std)
#>               NARTIC       PCTGRT      PCTSUPP
#> NARTIC   0.006172168 -0.003602529 -0.001943469
#> PCTGRT  -0.003602529  0.006699155 -0.002443584
#> PCTSUPP -0.001943469 -0.002443584  0.007311625
confint(std, level = 0.95)
#>              2.5 %    97.5 %
#> NARTIC  0.33659828 0.6536920
#> PCTGRT  0.22631203 0.5566654
#> PCTSUPP 0.09068548 0.4358099
```
