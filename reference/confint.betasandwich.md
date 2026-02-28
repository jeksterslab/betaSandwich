# Confidence Intervals for Standardized Regression Slopes

Confidence Intervals for Standardized Regression Slopes

## Usage

``` r
# S3 method for class 'betasandwich'
confint(object, parm = NULL, level = 0.95, ...)
```

## Arguments

- object:

  Object of class `betasandwich`.

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
confint(std, level = 0.95)
#>              2.5 %    97.5 %
#> NARTIC  0.33659828 0.6536920
#> PCTGRT  0.22631203 0.5566654
#> PCTSUPP 0.09068548 0.4358099
```
