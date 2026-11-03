# Estimate Semipartial Correlation Coefficients and the Corresponding Sampling Covariance Matrix

Estimate Semipartial Correlation Coefficients and the Corresponding
Sampling Covariance Matrix

## Usage

``` r
SCorSandwich(object, alpha = c(0.05, 0.01, 0.001))
```

## Arguments

- object:

  Object of class `betasandwich`, that is, the output of the
  [`BetaHC()`](https://github.com/jeksterslab/betaSandwich/reference/BetaHC.md),
  [`BetaN()`](https://github.com/jeksterslab/betaSandwich/reference/BetaN.md),
  or
  [`BetaADF()`](https://github.com/jeksterslab/betaSandwich/reference/BetaADF.md)
  functions.

- alpha:

  Numeric vector. Significance level \\(\alpha\\).

## Value

Returns an object of class `essandwich` which is a list with the
following elements:

- call:

  Function call.

- fit:

  The argument `object`.

- args:

  Function arguments.

- vcov:

  Sampling covariance matrix of semipartial correlation coefficients.

- est:

  Vector of semipartial correlation coefficients.

- fun:

  Function used ("SCorSandwich").

## Details

The sampling covariance matrix is obtained by the delta method from the
sampling covariance matrix of the standardized parameters stored in
`object` (normal-theory, asymptotically distribution-free, or
heteroskedasticity-consistent). Two or more regressors are required.

## See also

Other Beta Sandwich Functions:
[`BetaADF()`](https://github.com/jeksterslab/betaSandwich/reference/BetaADF.md),
[`BetaHC()`](https://github.com/jeksterslab/betaSandwich/reference/BetaHC.md),
[`BetaN()`](https://github.com/jeksterslab/betaSandwich/reference/BetaN.md),
[`DeltaRSqSandwich()`](https://github.com/jeksterslab/betaSandwich/reference/DeltaRSqSandwich.md),
[`DiffBetaSandwich()`](https://github.com/jeksterslab/betaSandwich/reference/DiffBetaSandwich.md),
[`FSqSandwich()`](https://github.com/jeksterslab/betaSandwich/reference/FSqSandwich.md),
[`PCorSandwich()`](https://github.com/jeksterslab/betaSandwich/reference/PCorSandwich.md),
[`RSqSandwich()`](https://github.com/jeksterslab/betaSandwich/reference/RSqSandwich.md)

## Author

Ivan Jacob Agaloos Pesigan

## Examples

``` r
object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
std <- BetaHC(object)
out <- SCorSandwich(std)
# Methods -------------------------------------------------------
print(out)
#> Call:
#> SCorSandwich(object = std)
#> 
#> Semipartial correlations with HC3 standard errors:
#>            est     se      z      p   0.05%   0.5%   2.5%  97.5%  99.5% 99.95%
#> NARTIC  0.4312 0.0794 5.4303 0.0000  0.1699 0.2266 0.2755 0.5868 0.6357 0.6924
#> PCTGRT  0.3430 0.0789 4.3468 0.0000  0.0834 0.1397 0.1883 0.4977 0.5463 0.6027
#> PCTSUPP 0.2385 0.0783 3.0465 0.0023 -0.0191 0.0368 0.0851 0.3919 0.4401 0.4961
summary(out)
#> Call:
#> SCorSandwich(object = std)
#> 
#> Semipartial correlations with HC3 standard errors:
#>            est     se      z      p   0.05%   0.5%   2.5%  97.5%  99.5% 99.95%
#> NARTIC  0.4312 0.0794 5.4303 0.0000  0.1699 0.2266 0.2755 0.5868 0.6357 0.6924
#> PCTGRT  0.3430 0.0789 4.3468 0.0000  0.0834 0.1397 0.1883 0.4977 0.5463 0.6027
#> PCTSUPP 0.2385 0.0783 3.0465 0.0023 -0.0191 0.0368 0.0851 0.3919 0.4401 0.4961
coef(out)
#>    NARTIC    PCTGRT   PCTSUPP 
#> 0.4311525 0.3430075 0.2384789 
vcov(out)
#>                NARTIC        PCTGRT       PCTSUPP
#> NARTIC   0.0063040434 -0.0002532449 -0.0004374935
#> PCTGRT  -0.0002532449  0.0062268718 -0.0010139363
#> PCTSUPP -0.0004374935 -0.0010139363  0.0061275595
confint(out, level = 0.95)
#>              2.5 %    97.5 %
#> NARTIC  0.27553526 0.5867697
#> PCTGRT  0.18834573 0.4976693
#> PCTSUPP 0.08505544 0.3919024
```
