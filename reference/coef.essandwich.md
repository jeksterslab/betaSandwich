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
# Data ---------------------------------------------------------------------
data("nas1982", package = "betaSandwich")

# Fit Model in lm ----------------------------------------------------------
object <- stats::lm(
  formula = QUALITY ~ NARTIC + PCTGRT + PCTSUPP,
  data = nas1982
)

# BetaHC -------------------------------------------------------------------
std <- BetaHC(object = object)

# RSqSandwich --------------------------------------------------------------
out <- RSqSandwich(object = std)

## Methods -----------------------------------------------------------------
coef(object = out)
#>       rsq       adj 
#> 0.8045263 0.7905638 
```
