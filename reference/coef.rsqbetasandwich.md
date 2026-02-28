# Multiple Correlation Coefficients (R-Squared and Adjusted R-Squared)

Multiple Correlation Coefficients (R-Squared and Adjusted R-Squared)

## Usage

``` r
# S3 method for class 'rsqbetasandwich'
coef(object, ...)
```

## Arguments

- object:

  Object of class `rsqbetasandwich`.

- ...:

  additional arguments.

## Value

Returns a vector of multiple correlation coefficients (R-squared and
adjusted R-squared)

## Author

Ivan Jacob Agaloos Pesigan

## Examples

``` r
object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
std <- BetaHC(object)
rsq <- RSqBetaSandwich(std)
coef(rsq)
#>   rsq.rsq   adj.adj 
#> 0.8045263 0.7905638 
```
