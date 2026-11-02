# Sampling Covariance Matrix of Multiple Correlation Coefficients (R-Squared and Adjusted R-Squared)

Sampling Covariance Matrix of Multiple Correlation Coefficients
(R-Squared and Adjusted R-Squared)

## Usage

``` r
# S3 method for class 'rsqbetasandwich'
vcov(object, ...)
```

## Arguments

- object:

  Object of class `rsqbetasandwich`.

- ...:

  additional arguments.

## Value

Returns a matrix of the variance-covariance matrix of multiple
correlation coefficients (R-squared and adjusted R-squared).

## Author

Ivan Jacob Agaloos Pesigan

## Examples

``` r
object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
std <- BetaHC(object)
rsq <- RSqBetaSandwich(std)
vcov(rsq)
#>             rsq         adj
#> rsq 0.003288894 0.003523815
#> adj 0.003523815 0.003775516
```
