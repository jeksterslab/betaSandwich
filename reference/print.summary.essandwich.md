# Print Method for an Object of Class `summary.essandwich`

Print Method for an Object of Class `summary.essandwich`

## Usage

``` r
# S3 method for class 'summary.essandwich'
print(x, ...)
```

## Arguments

- x:

  Object of class `summary.essandwich`.

- ...:

  additional arguments.

## Value

Prints a matrix of estimates, standard errors, test statistics,
p-values, and confidence intervals.

## Author

Ivan Jacob Agaloos Pesigan

## Examples

``` r
object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
std <- BetaHC(object)
out <- RSqSandwich(std)
summary(out)
#> Call:
#> RSqSandwich(object = std)
#> 
#> R-squared and adjusted R-squared with HC3 standard errors:
#>        est     se       z p  0.05%   0.5%   2.5%  97.5%  99.5% 99.95%
#> rsq 0.8045 0.0573 14.0286 0 0.6158 0.6568 0.6921 0.9169 0.9522 0.9932
#> adj 0.7906 0.0614 12.8662 0 0.5884 0.6323 0.6701 0.9110 0.9488 0.9928
```
