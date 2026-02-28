# Estimate Differences of Standardized Slopes and the Corresponding Sampling Covariance Matrix

Estimate Differences of Standardized Slopes and the Corresponding
Sampling Covariance Matrix

## Usage

``` r
DiffBetaSandwich(object, alpha = c(0.05, 0.01, 0.001))
```

## Arguments

- object:

  Object of class `betasandwich`, that is, the output of the
  [`BetaHC()`](https://github.com/jeksterslab/betaSandwich/reference/BetaHC.md),
  [`BetaN()`](https://github.com/jeksterslab/betaSandwich/reference/BetaN.md),
  or
  [`BetaADF()`](https://github.com/jeksterslab/betaSandwich/reference/BetaADF.md)
  functions.

- alpha:

  Numeric vector. Significance level \\\alpha\\.

## Value

Returns an object of class `diffbetasandwich` which is a list with the
following elements:

- call:

  Function call.

- fit:

  The argument `object`.

- args:

  Function arguments.

- vcov:

  Sampling covariance matrix of differences of standardized slopes.

- est:

  Vector of differences of standardized slopes.

## See also

Other Beta Sandwich Functions:
[`BetaADF()`](https://github.com/jeksterslab/betaSandwich/reference/BetaADF.md),
[`BetaHC()`](https://github.com/jeksterslab/betaSandwich/reference/BetaHC.md),
[`BetaN()`](https://github.com/jeksterslab/betaSandwich/reference/BetaN.md),
[`RSqBetaSandwich()`](https://github.com/jeksterslab/betaSandwich/reference/RSqBetaSandwich.md)

## Author

Ivan Jacob Agaloos Pesigan

## Examples

``` r
object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
std <- BetaHC(object)
diff <- DiffBetaSandwich(std)
# Methods -------------------------------------------------------
print(diff)
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
coef(diff)
#>  NARTIC-PCTGRT NARTIC-PCTSUPP PCTGRT-PCTSUPP 
#>      0.1036564      0.2318974      0.1282410 
vcov(diff)
#>                NARTIC-PCTGRT NARTIC-PCTSUPP PCTGRT-PCTSUPP
#> NARTIC-PCTGRT    0.020076382    0.009274583   -0.010801799
#> NARTIC-PCTSUPP   0.009274583    0.017370731    0.008096148
#> PCTGRT-PCTSUPP  -0.010801799    0.008096148    0.018897947
confint(diff, level = 0.95)
#>                      2.5 %    97.5 %
#> NARTIC-PCTGRT  -0.17405314 0.3813660
#> NARTIC-PCTSUPP -0.02642203 0.4902169
#> PCTGRT-PCTSUPP -0.14119483 0.3976769
```
