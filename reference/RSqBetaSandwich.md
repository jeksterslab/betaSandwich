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

  Numeric vector. Significance level \\\alpha\\.

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
#>        est     se       t df p  0.05% 0.5%   2.5%  97.5%  99.5% 99.95%
#> rsq 0.8045 0.0313 25.6916 42 0 0.6937 0.72 0.7413 0.8677 0.8890 0.9153
#> adj 0.7906 0.0336 23.5627 42 0 0.6719 0.70 0.7229 0.8583 0.8811 0.9093
summary(rsq)
#> Call:
#> RSqBetaSandwich(object = std)
#> 
#> Multiple correlation with HC3 standard errors:
#>        est     se       t df p  0.05% 0.5%   2.5%  97.5%  99.5% 99.95%
#> rsq 0.8045 0.0313 25.6916 42 0 0.6937 0.72 0.7413 0.8677 0.8890 0.9153
#> adj 0.7906 0.0336 23.5627 42 0 0.6719 0.70 0.7229 0.8583 0.8811 0.9093
coef(rsq)
#>   rsq.rsq   adj.adj 
#> 0.8045263 0.7905638 
vcov(rsq)
#>              rsq         adj
#> rsq 0.0009806163 0.001050660
#> adj 0.0010506603 0.001125707
confint(rsq, level = 0.95)
#>         2.5 %    97.5 %
#> rsq 0.7413304 0.8677221
#> adj 0.7228540 0.8582736
```
