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

  Numeric vector. Significance level \\\alpha\\. If `alpha = NULL`, use
  the argument `alpha` used in `x`.

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
#>        est     se       t df p  0.05% 0.5%   2.5%  97.5%  99.5% 99.95%
#> rsq 0.8045 0.0313 25.6916 42 0 0.6937 0.72 0.7413 0.8677 0.8890 0.9153
#> adj 0.7906 0.0336 23.5627 42 0 0.6719 0.70 0.7229 0.8583 0.8811 0.9093
```
