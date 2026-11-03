# Summary Method for an Object of Class `essandwich`

Summary Method for an Object of Class `essandwich`

## Usage

``` r
# S3 method for class 'essandwich'
summary(object, alpha = NULL, digits = 4, ...)
```

## Arguments

- object:

  Object of class `essandwich`.

- alpha:

  Numeric vector. Significance level \\(\alpha\\). If `alpha = NULL`,
  use the argument `alpha` used in `object`.

- digits:

  Digits to print.

- ...:

  additional arguments.

## Value

Returns a matrix of estimates, standard errors, test statistics,
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
