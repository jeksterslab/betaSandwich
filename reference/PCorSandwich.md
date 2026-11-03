# Estimate Squared Partial Correlation Coefficients and the Corresponding Sampling Covariance Matrix

Estimate Squared Partial Correlation Coefficients and the Corresponding
Sampling Covariance Matrix

## Usage

``` r
PCorSandwich(object, alpha = c(0.05, 0.01, 0.001))
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

  Sampling covariance matrix of squared partial correlation
  coefficients.

- est:

  Vector of squared partial correlation coefficients.

- fun:

  Function used ("PCorSandwich").

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
[`RSqSandwich()`](https://github.com/jeksterslab/betaSandwich/reference/RSqSandwich.md),
[`SCorSandwich()`](https://github.com/jeksterslab/betaSandwich/reference/SCorSandwich.md)

## Author

Ivan Jacob Agaloos Pesigan

## Examples

``` r
object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
std <- BetaHC(object)
out <- PCorSandwich(std)
# Methods -------------------------------------------------------
print(out)
#> Call:
#> PCorSandwich(object = std)
#> 
#> Squared partial correlations with HC3 standard errors:
#>            est     se      z      p   0.05%    0.5%    2.5%  97.5%  99.5%
#> NARTIC  0.4874 0.1095 4.4532 0.0000  0.1273  0.2055  0.2729 0.7020 0.7694
#> PCTGRT  0.3757 0.1156 3.2500 0.0012 -0.0047  0.0779  0.1491 0.6023 0.6735
#> PCTSUPP 0.2254 0.1277 1.7647 0.0776 -0.1949 -0.1036 -0.0249 0.4757 0.5543
#>         99.95%
#> NARTIC  0.8476
#> PCTGRT  0.7562
#> PCTSUPP 0.6456
summary(out)
#> Call:
#> PCorSandwich(object = std)
#> 
#> Squared partial correlations with HC3 standard errors:
#>            est     se      z      p   0.05%    0.5%    2.5%  97.5%  99.5%
#> NARTIC  0.4874 0.1095 4.4532 0.0000  0.1273  0.2055  0.2729 0.7020 0.7694
#> PCTGRT  0.3757 0.1156 3.2500 0.0012 -0.0047  0.0779  0.1491 0.6023 0.6735
#> PCTSUPP 0.2254 0.1277 1.7647 0.0776 -0.1949 -0.1036 -0.0249 0.4757 0.5543
#>         99.95%
#> NARTIC  0.8476
#> PCTGRT  0.7562
#> PCTSUPP 0.6456
coef(out)
#>    NARTIC    PCTGRT   PCTSUPP 
#> 0.4874382 0.3757383 0.2253739 
vcov(out)
#>              NARTIC       PCTGRT      PCTSUPP
#> NARTIC  0.011980986 0.0021633722 0.0027577200
#> PCTGRT  0.002163372 0.0133662295 0.0007428294
#> PCTSUPP 0.002757720 0.0007428294 0.0163111322
confint(out, level = 0.95)
#>               2.5 %    97.5 %
#> NARTIC   0.27290509 0.7019714
#> PCTGRT   0.14914218 0.6023345
#> PCTSUPP -0.02494295 0.4756908
```
