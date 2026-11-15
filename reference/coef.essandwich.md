# Estimates for an Object of Class `essandwich`

Estimates for an Object of Class `essandwich`

## Usage

``` r
# S3 method for class 'essandwich'
coef(object, ...)
```

## Arguments

- object:

  Object of class `essandwich`.

- ...:

  additional arguments.

## Value

Returns a vector of estimates.

## Author

Ivan Jacob Agaloos Pesigan

## Examples

``` r
object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
std <- BetaHC(object)
out <- RSqSandwich(std)
coef(out)
#>       rsq       adj 
#> 0.8045263 0.7905638 
```
