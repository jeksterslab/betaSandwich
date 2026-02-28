# Print Method for an Object of Class `betasandwich`

Print Method for an Object of Class `betasandwich`

## Usage

``` r
# S3 method for class 'betasandwich'
print(x, alpha = NULL, digits = 4, ...)
```

## Arguments

- x:

  Object of class `betasandwich`.

- alpha:

  Numeric vector. Significance level \\\alpha\\. If `alpha = NULL`, use
  the argument `alpha` used in `x`.

- digits:

  Digits to print.

- ...:

  additional arguments.

## Value

Prints a matrix of standardized regression slopes, standard errors, test
statistics, degrees of freedom, p-values, and confidence intervals.

## Author

Ivan Jacob Agaloos Pesigan

## Examples

``` r
object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
std <- BetaHC(object)
print(std)
#> Call:
#> BetaHC(object = object)
#> 
#> Standardized regression slopes with HC3 standard errors:
#>            est     se      t df      p   0.05%   0.5%   2.5%  97.5%  99.5%
#> NARTIC  0.4951 0.0786 6.3025 42 0.0000  0.2172 0.2832 0.3366 0.6537 0.7071
#> PCTGRT  0.3915 0.0818 4.7831 42 0.0000  0.1019 0.1707 0.2263 0.5567 0.6123
#> PCTSUPP 0.2632 0.0855 3.0786 42 0.0037 -0.0393 0.0325 0.0907 0.4358 0.4940
#>         99.95%
#> NARTIC  0.7731
#> PCTGRT  0.6810
#> PCTSUPP 0.5658
```
