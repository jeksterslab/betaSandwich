# Estimate R-Squared and Adjusted R-Squared and the Corresponding Sampling Covariance Matrix

Estimate R-Squared and Adjusted R-Squared and the Corresponding Sampling
Covariance Matrix

## Usage

``` r
RSqSandwich(object, alpha = c(0.05, 0.01, 0.001))
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

  Sampling covariance matrix of R-squared and adjusted R-squared.

- est:

  Vector of R-squared and adjusted R-squared.

- fun:

  Function used ("RSqSandwich").

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
[`FSqSandwich()`](https://github.com/jeksterslab/betaSandwich/reference/FSqSandwich.md),
[`PCorSandwich()`](https://github.com/jeksterslab/betaSandwich/reference/PCorSandwich.md),
[`SCorSandwich()`](https://github.com/jeksterslab/betaSandwich/reference/SCorSandwich.md)

## Author

Ivan Jacob Agaloos Pesigan

## Examples

``` r
object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
std <- BetaHC(object)
out <- RSqSandwich(std)
# Methods -------------------------------------------------------
print(out)
#> Call:
#> RSqSandwich(object = std)
#> 
#> R-squared and adjusted R-squared with HC3 standard errors:
#>        est     se       z p  0.05%   0.5%   2.5%  97.5%  99.5% 99.95%
#> rsq 0.8045 0.0573 14.0286 0 0.6158 0.6568 0.6921 0.9169 0.9522 0.9932
#> adj 0.7906 0.0614 12.8662 0 0.5884 0.6323 0.6701 0.9110 0.9488 0.9928
summary(out)
#> Call:
#> RSqSandwich(object = std)
#> 
#> R-squared and adjusted R-squared with HC3 standard errors:
#>        est     se       z p  0.05%   0.5%   2.5%  97.5%  99.5% 99.95%
#> rsq 0.8045 0.0573 14.0286 0 0.6158 0.6568 0.6921 0.9169 0.9522 0.9932
#> adj 0.7906 0.0614 12.8662 0 0.5884 0.6323 0.6701 0.9110 0.9488 0.9928
coef(out)
#>       rsq       adj 
#> 0.8045263 0.7905638 
vcov(out)
#>             rsq         adj
#> rsq 0.003288894 0.003523815
#> adj 0.003523815 0.003775516
confint(out, level = 0.95)
#>         2.5 %    97.5 %
#> rsq 0.6921245 0.9169280
#> adj 0.6701334 0.9109943
```
