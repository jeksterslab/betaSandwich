# Confidence Intervals for Multiple Correlation Coefficients (R-Squared and Adjusted R-Squared)

Confidence Intervals for Multiple Correlation Coefficients (R-Squared
and Adjusted R-Squared)

## Usage

``` r
# S3 method for class 'rsqbetasandwich'
confint(object, parm = NULL, level = 0.95, ...)
```

## Arguments

- object:

  Object of class `rsqbetasandwich`.

- parm:

  a specification of which parameters are to be given confidence
  intervals, either a vector of numbers or a vector of names. If
  missing, all parameters are considered.

- level:

  the confidence level required.

- ...:

  additional arguments.

## Value

Returns a matrix of confidence intervals.

## Author

Ivan Jacob Agaloos Pesigan

## Examples

``` r
object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
std <- BetaHC(object)
rsq <- RSqBetaSandwich(std)
confint(rsq, level = 0.95)
#>         2.5 %    97.5 %
#> rsq 0.7413304 0.8677221
#> adj 0.7228540 0.8582736
```
