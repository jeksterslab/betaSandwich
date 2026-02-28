# Sampling Covariance Matrix of Differences of Standardized Regression Slopes

Sampling Covariance Matrix of Differences of Standardized Regression
Slopes

## Usage

``` r
# S3 method for class 'diffbetasandwich'
vcov(object, ...)
```

## Arguments

- object:

  Object of class `diffbetasandwich`.

- ...:

  additional arguments.

## Value

Returns a matrix of the variance-covariance matrix of differences of
standardized regression slopes.

## Author

Ivan Jacob Agaloos Pesigan

## Examples

``` r
object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
std <- BetaHC(object)
diff <- DiffBetaSandwich(std)
vcov(diff)
#>                NARTIC-PCTGRT NARTIC-PCTSUPP PCTGRT-PCTSUPP
#> NARTIC-PCTGRT    0.020076382    0.009274583   -0.010801799
#> NARTIC-PCTSUPP   0.009274583    0.017370731    0.008096148
#> PCTGRT-PCTSUPP  -0.010801799    0.008096148    0.018897947
```
