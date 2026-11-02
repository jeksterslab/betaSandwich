# Estimate Multiple Correlation Coefficients (R-squared and adjusted R-squared) and the Corresponding Sampling Covariance Matrix

Estimate Multiple Correlation Coefficients (R-squared and adjusted
R-squared) and the Corresponding Sampling Covariance Matrix

## Usage

``` r
RSqBetaSandwich(object, alpha = c(0.05, 0.01, 0.001))
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

Returns an object of class `rsqbetasandwich` which is a list with the
following elements:

- call:

  Function call.

- fit:

  The argument `object`.

- args:

  Function arguments.

- vcov:

  Sampling covariance matrix of multiple correlation coefficients
  (R-squared and adjusted R-squared).

- est:

  Vector of multiple correlation coefficients (R-squared and adjusted
  R-squared).

## See also

Other Beta Sandwich Functions:
[`BetaADF()`](https://github.com/jeksterslab/betaSandwich/reference/BetaADF.md),
[`BetaHC()`](https://github.com/jeksterslab/betaSandwich/reference/BetaHC.md),
[`BetaN()`](https://github.com/jeksterslab/betaSandwich/reference/BetaN.md),
[`DiffBetaSandwich()`](https://github.com/jeksterslab/betaSandwich/reference/DiffBetaSandwich.md)

## Author

Ivan Jacob Agaloos Pesigan

## Examples

``` r
object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
std <- BetaHC(object)
rsq <- RSqBetaSandwich(std)
# Methods -------------------------------------------------------
print(rsq)
#> Call:
#> RSqBetaSandwich(object = std)
#> 
#> Multiple correlation with HC3 standard errors:
#>        est     se       t df p  0.05%   0.5%   2.5%  97.5%  99.5% 99.95%
#> rsq 0.8045 0.0573 14.0286 42 0 0.6016 0.6498 0.6888 0.9203 0.9593 1.0074
#> adj 0.7906 0.0614 12.8662 42 0 0.5732 0.6248 0.6666 0.9146 0.9563 1.0079
summary(rsq)
#> Call:
#> RSqBetaSandwich(object = std)
#> 
#> Multiple correlation with HC3 standard errors:
#>        est     se       t df p  0.05%   0.5%   2.5%  97.5%  99.5% 99.95%
#> rsq 0.8045 0.0573 14.0286 42 0 0.6016 0.6498 0.6888 0.9203 0.9593 1.0074
#> adj 0.7906 0.0614 12.8662 42 0 0.5732 0.6248 0.6666 0.9146 0.9563 1.0079
coef(rsq)
#>   rsq.rsq   adj.adj 
#> 0.8045263 0.7905638 
vcov(rsq)
#>             rsq         adj
#> rsq 0.003288894 0.003523815
#> adj 0.003523815 0.003775516
confint(rsq, level = 0.95)
#>         2.5 %    97.5 %
#> rsq 0.6887915 0.9202610
#> adj 0.6665624 0.9145653
```
