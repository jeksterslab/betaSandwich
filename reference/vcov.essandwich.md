# Sampling Covariance Matrix for an Object of Class `essandwich`

Sampling Covariance Matrix for an Object of Class `essandwich`

## Usage

``` r
# S3 method for class 'essandwich'
vcov(object, ...)
```

## Arguments

- object:

  Object of class `essandwich`.

- ...:

  additional arguments.

## Value

Returns a matrix of the variance-covariance matrix of the estimates.

## Author

Ivan Jacob Agaloos Pesigan

## Examples

``` r
object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
std <- BetaHC(object)
out <- RSqSandwich(std)
vcov(out)
#>             rsq         adj
#> rsq 0.003288894 0.003523815
#> adj 0.003523815 0.003775516
```
