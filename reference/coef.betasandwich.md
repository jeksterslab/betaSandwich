# Standardized Regression Slopes

Standardized Regression Slopes

## Usage

``` r
# S3 method for class 'betasandwich'
coef(object, ...)
```

## Arguments

- object:

  Object of class `betasandwich`.

- ...:

  additional arguments.

## Value

Returns a vector of standardized regression slopes.

## Author

Ivan Jacob Agaloos Pesigan

## Examples

``` r
object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
std <- BetaHC(object)
coef(std)
#>    NARTIC    PCTGRT   PCTSUPP 
#> 0.4951451 0.3914887 0.2632477 
```
