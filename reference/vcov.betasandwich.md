# Sampling Covariance Matrix of the Standardized Regression Slopes

Sampling Covariance Matrix of the Standardized Regression Slopes

## Usage

``` r
# S3 method for class 'betasandwich'
vcov(object, ...)
```

## Arguments

- object:

  Object of class `betasandwich`.

- ...:

  additional arguments.

## Value

Returns a matrix of the variance-covariance matrix of standardized
slopes.

## Author

Ivan Jacob Agaloos Pesigan

## Examples

``` r
object <- stats::lm(
  formula = QUALITY ~ NARTIC + PCTGRT + PCTSUPP,
  data = nas1982
)
std <- BetaHC(object = object)
vcov(object = std)
#>               NARTIC       PCTGRT      PCTSUPP
#> NARTIC   0.006172168 -0.003602529 -0.001943469
#> PCTGRT  -0.003602529  0.006699155 -0.002443584
#> PCTSUPP -0.001943469 -0.002443584  0.007311625
```
