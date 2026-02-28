# Confidence Intervals for Differences of Standardized Regression Slopes

Confidence Intervals for Differences of Standardized Regression Slopes

## Usage

``` r
# S3 method for class 'diffbetasandwich'
confint(object, parm = NULL, level = 0.95, ...)
```

## Arguments

- object:

  Object of class `diffbetasandwich`.

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
diff <- DiffBetaSandwich(std)
confint(diff, level = 0.95)
#>                      2.5 %    97.5 %
#> NARTIC-PCTGRT  -0.17405314 0.3813660
#> NARTIC-PCTSUPP -0.02642203 0.4902169
#> PCTGRT-PCTSUPP -0.14119483 0.3976769
```
