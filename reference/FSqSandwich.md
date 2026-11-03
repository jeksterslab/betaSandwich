# Estimate Cohen's f-Squared and Local f-Squared and the Corresponding Sampling Covariance Matrix

Estimate Cohen's f-Squared and Local f-Squared and the Corresponding
Sampling Covariance Matrix

## Usage

``` r
FSqSandwich(object, alpha = c(0.05, 0.01, 0.001))
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

  Sampling covariance matrix of Cohen's f-squared (fsq) and the local
  f-squared of each regressor.

- est:

  Vector of Cohen's f-squared (fsq) and the local f-squared of each
  regressor.

- fun:

  Function used ("FSqSandwich").

## Details

The sampling covariance matrix is obtained by the delta method from the
sampling covariance matrix of the standardized parameters stored in
`object` (normal-theory, asymptotically distribution-free, or
heteroskedasticity-consistent).

## See also

Other Beta Sandwich Functions:
[`BetaADF()`](https://github.com/jeksterslab/betaSandwich/reference/BetaADF.md),
[`BetaHC()`](https://github.com/jeksterslab/betaSandwich/reference/BetaHC.md),
[`BetaN()`](https://github.com/jeksterslab/betaSandwich/reference/BetaN.md),
[`DeltaRSqSandwich()`](https://github.com/jeksterslab/betaSandwich/reference/DeltaRSqSandwich.md),
[`DiffBetaSandwich()`](https://github.com/jeksterslab/betaSandwich/reference/DiffBetaSandwich.md),
[`PCorSandwich()`](https://github.com/jeksterslab/betaSandwich/reference/PCorSandwich.md),
[`RSqSandwich()`](https://github.com/jeksterslab/betaSandwich/reference/RSqSandwich.md),
[`SCorSandwich()`](https://github.com/jeksterslab/betaSandwich/reference/SCorSandwich.md)

## Author

Ivan Jacob Agaloos Pesigan

## Examples

``` r
object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
std <- BetaHC(object)
out <- FSqSandwich(std)
# Methods -------------------------------------------------------
print(out)
#> Call:
#> FSqSandwich(object = std)
#> 
#> Cohen's f-squared (fsq) and local f-squared with HC3 standard errors:
#>            est     se      z      p   0.05%    0.5%    2.5%  97.5%  99.5%
#> fsq     4.1158 1.5009 2.7422 0.0061 -0.8229  0.2497  1.1741 7.0575 7.9818
#> NARTIC  0.9510 0.4166 2.2825 0.0225 -0.4200 -0.1222  0.1344 1.7676 2.0242
#> PCTGRT  0.6019 0.2967 2.0288 0.0425 -0.3743 -0.1623  0.0204 1.1834 1.3661
#> PCTSUPP 0.2909 0.2128 1.3670 0.1716 -0.4094 -0.2573 -0.1262 0.7081 0.8392
#>         99.95%
#> fsq     9.0545
#> NARTIC  2.3219
#> PCTGRT  1.5781
#> PCTSUPP 0.9913
summary(out)
#> Call:
#> FSqSandwich(object = std)
#> 
#> Cohen's f-squared (fsq) and local f-squared with HC3 standard errors:
#>            est     se      z      p   0.05%    0.5%    2.5%  97.5%  99.5%
#> fsq     4.1158 1.5009 2.7422 0.0061 -0.8229  0.2497  1.1741 7.0575 7.9818
#> NARTIC  0.9510 0.4166 2.2825 0.0225 -0.4200 -0.1222  0.1344 1.7676 2.0242
#> PCTGRT  0.6019 0.2967 2.0288 0.0425 -0.3743 -0.1623  0.0204 1.1834 1.3661
#> PCTSUPP 0.2909 0.2128 1.3670 0.1716 -0.4094 -0.2573 -0.1262 0.7081 0.8392
#>         99.95%
#> fsq     9.0545
#> NARTIC  2.3219
#> PCTGRT  1.5781
#> PCTSUPP 0.9913
coef(out)
#>       fsq    NARTIC    PCTGRT   PCTSUPP 
#> 4.1157764 0.9509843 0.6018924 0.2909455 
vcov(out)
#>               fsq     NARTIC      PCTGRT     PCTSUPP
#> fsq     2.2526623 0.34629953 0.180617270 0.141603544
#> NARTIC  0.3462995 0.17358320 0.021130291 0.017493371
#> PCTGRT  0.1806173 0.02113029 0.088012079 0.003176666
#> PCTSUPP 0.1416035 0.01749337 0.003176666 0.045301824
confint(out, level = 0.95)
#>               2.5 %    97.5 %
#> fsq      1.17409165 7.0574612
#> NARTIC   0.13439832 1.7675703
#> PCTGRT   0.02043322 1.1833516
#> PCTSUPP -0.12621769 0.7081086
```
