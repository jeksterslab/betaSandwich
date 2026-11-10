# Estimate Improvement in R-Squared and the Corresponding Sampling Covariance Matrix

Estimate Improvement in R-Squared and the Corresponding Sampling
Covariance Matrix

## Usage

``` r
DeltaRSqSandwich(object, alpha = c(0.05, 0.01, 0.001))
```

## Arguments

- object:

  Object of class `betasandwich`, that is, the output of the
  [`BetaHC()`](https://jeksterslab.github.io/betaSandwich/reference/BetaHC.md),
  [`BetaN()`](https://jeksterslab.github.io/betaSandwich/reference/BetaN.md),
  or
  [`BetaADF()`](https://jeksterslab.github.io/betaSandwich/reference/BetaADF.md)
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

  Sampling covariance matrix of improvement in R-squared (squared
  semipartial correlation coefficients).

- est:

  Vector of improvement in R-squared (squared semipartial correlation
  coefficients).

- fun:

  Function used ("DeltaRSqSandwich").

## Details

The sampling covariance matrix is obtained by the delta method from the
sampling covariance matrix of the standardized parameters stored in
`object` (normal-theory, asymptotically distribution-free, or
heteroskedasticity-consistent). Two or more regressors are required.

## See also

Other Beta Sandwich Functions:
[`BetaADF()`](https://jeksterslab.github.io/betaSandwich/reference/BetaADF.md),
[`BetaHC()`](https://jeksterslab.github.io/betaSandwich/reference/BetaHC.md),
[`BetaN()`](https://jeksterslab.github.io/betaSandwich/reference/BetaN.md),
[`DiffBetaSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/DiffBetaSandwich.md),
[`FSqSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/FSqSandwich.md),
[`PCorSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/PCorSandwich.md),
[`RSqSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/RSqSandwich.md),
[`SCorSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/SCorSandwich.md)

## Author

Ivan Jacob Agaloos Pesigan

## Examples

``` r
object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
std <- BetaHC(object)
out <- DeltaRSqSandwich(std)
# Methods -------------------------------------------------------
print(out)
#> Call:
#> DeltaRSqSandwich(object = std)
#> 
#> Improvement in R-squared with HC3 standard errors:
#>            est     se      z      p   0.05%    0.5%    2.5%  97.5%  99.5%
#> NARTIC  0.1859 0.0685 2.7151 0.0066 -0.0394  0.0095  0.0517 0.3201 0.3622
#> PCTGRT  0.1177 0.0541 2.1734 0.0298 -0.0605 -0.0218  0.0116 0.2238 0.2571
#> PCTSUPP 0.0569 0.0373 1.5233 0.1277 -0.0660 -0.0393 -0.0163 0.1300 0.1530
#>         99.95%
#> NARTIC  0.4112
#> PCTGRT  0.2958
#> PCTSUPP 0.1797
summary(out)
#> Call:
#> DeltaRSqSandwich(object = std)
#> 
#> Improvement in R-squared with HC3 standard errors:
#>            est     se      z      p   0.05%    0.5%    2.5%  97.5%  99.5%
#> NARTIC  0.1859 0.0685 2.7151 0.0066 -0.0394  0.0095  0.0517 0.3201 0.3622
#> PCTGRT  0.1177 0.0541 2.1734 0.0298 -0.0605 -0.0218  0.0116 0.2238 0.2571
#> PCTSUPP 0.0569 0.0373 1.5233 0.1277 -0.0660 -0.0393 -0.0163 0.1300 0.1530
#>         99.95%
#> NARTIC  0.4112
#> PCTGRT  0.2958
#> PCTSUPP 0.1797
coef(out)
#>    NARTIC    PCTGRT   PCTSUPP 
#> 0.1858925 0.1176542 0.0568722 
vcov(out)
#>                NARTIC        PCTGRT       PCTSUPP
#> NARTIC   0.0046874969 -0.0001498081 -0.0001799337
#> PCTGRT  -0.0001498081  0.0029304696 -0.0003317602
#> PCTSUPP -0.0001799337 -0.0003317602  0.0013939512
confint(out, level = 0.95)
#>               2.5 %    97.5 %
#> NARTIC   0.05170296 0.3200820
#> PCTGRT   0.01155384 0.2237545
#> PCTSUPP -0.01630434 0.1300487
```
