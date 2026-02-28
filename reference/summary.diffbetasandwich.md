# Summary Method for an Object of Class `diffbetasandwich`

Summary Method for an Object of Class `diffbetasandwich`

## Usage

``` r
# S3 method for class 'diffbetasandwich'
summary(object, alpha = NULL, digits = 4, ...)
```

## Arguments

- object:

  Object of class `diffbetasandwich`.

- alpha:

  Numeric vector. Significance level \\\alpha\\. If `alpha = NULL`, use
  the argument `alpha` used in `object`.

- digits:

  Digits to print.

- ...:

  additional arguments.

## Value

Returns a matrix of differences of standardized regression slopes,
standard errors, test statistics, degrees of freedom, p-values, and
confidence intervals.

## Author

Ivan Jacob Agaloos Pesigan

## Examples

``` r
object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
std <- BetaHC(object)
diff <- DiffBetaSandwich(std)
summary(diff)
#> Call:
#> DiffBetaSandwich(object = std)
#> 
#> Difference between standardized regression coefficients with HC3 standard errors:
#>                   est     se      z      p   0.05%    0.5%    2.5%  97.5%
#> NARTIC-PCTGRT  0.1037 0.1417 0.7316 0.4644 -0.3626 -0.2613 -0.1741 0.3814
#> NARTIC-PCTSUPP 0.2319 0.1318 1.7595 0.0785 -0.2018 -0.1076 -0.0264 0.4902
#> PCTGRT-PCTSUPP 0.1282 0.1375 0.9329 0.3509 -0.3241 -0.2259 -0.1412 0.3977
#>                 99.5% 99.95%
#> NARTIC-PCTGRT  0.4686 0.5699
#> NARTIC-PCTSUPP 0.5714 0.6656
#> PCTGRT-PCTSUPP 0.4823 0.5806
```
