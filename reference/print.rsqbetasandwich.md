# Print Method for an Object of Class `rsqbetasandwich`

Print Method for an Object of Class `rsqbetasandwich`

## Usage

``` r
# S3 method for class 'rsqbetasandwich'
print(x, alpha = NULL, digits = 4, ...)
```

## Arguments

- x:

  Object of class `rsqbetasandwich`.

- alpha:

  Numeric vector. Significance level \\(\alpha\\). If `alpha = NULL`,
  use the argument `alpha` used in `x`.

- digits:

  Digits to print.

- ...:

  additional arguments.

## Value

Prints a matrix of multiple correlation coefficients (R-squared and
adjusted R-squared), standard errors, test statistics, degrees of
freedom, p-values, and confidence intervals.

## Author

Ivan Jacob Agaloos Pesigan

## Examples

``` r
object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
std <- BetaHC(object)
rsq <- RSqBetaSandwich(std)
print(rsq)
#> Call:
#> RSqBetaSandwich(object = std)
#> 
#> Multiple correlation with HC3 standard errors:
#>        est     se       t df p  0.05%   0.5%   2.5%  97.5%  99.5% 99.95%
#> rsq 0.8045 0.0573 14.0286 42 0 0.6016 0.6498 0.6888 0.9203 0.9593 1.0074
#> adj 0.7906 0.0614 12.8662 42 0 0.5732 0.6248 0.6666 0.9146 0.9563 1.0079
```
