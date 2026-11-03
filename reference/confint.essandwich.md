# Confidence Intervals for an Object of Class `essandwich`

Confidence Intervals for an Object of Class `essandwich`

## Usage

``` r
# S3 method for class 'essandwich'
confint(object, parm = NULL, level = 0.95, ...)
```

## Arguments

- object:

  Object of class `essandwich`.

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
out <- RSqSandwich(std)
confint(out, level = 0.95)
#>         2.5 %    97.5 %
#> rsq 0.6921245 0.9169280
#> adj 0.6701334 0.9109943
```
