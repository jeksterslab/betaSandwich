# Summary Method for an Object of Class `betasandwich`

Summary Method for an Object of Class `betasandwich`

## Usage

``` r
# S3 method for class 'betasandwich'
summary(object, alpha = NULL, digits = 4, ...)
```

## Arguments

- object:

  Object of class `betasandwich`.

- alpha:

  Numeric vector. Significance level \\(\alpha\\). If `alpha = NULL`,
  use the argument `alpha` used in `object`.

- digits:

  Digits to print.

- ...:

  additional arguments.

## Value

Returns a matrix of standardized regression slopes, standard errors,
test statistics, degrees of freedom, p-values, and confidence intervals.

## Details

The confidence intervals are \\(\hat{\theta} \pm t\_{1 - \alpha / 2, n -
k} \widehat{\operatorname{se}} \left( \hat{\theta} \right)\\), where
\\(\widehat{\operatorname{se}}\\) is the square root of the diagonal of
the sampling covariance matrix (see
[`BetaHC()`](https://jeksterslab.github.io/betaSandwich/reference/BetaHC.md),
[`BetaN()`](https://jeksterslab.github.io/betaSandwich/reference/BetaN.md),
and
[`BetaADF()`](https://jeksterslab.github.io/betaSandwich/reference/BetaADF.md)),
\\(n\\) is the sample size, and \\(k\\) is the number of regression
parameters, including the intercept. The test statistic is
\\(\hat{\theta} / \widehat{\operatorname{se}} \left( \hat{\theta}
\right)\\) with \\(n - k\\) degrees of freedom.

## Author

Ivan Jacob Agaloos Pesigan

## Examples

``` r
# Data ---------------------------------------------------------------------
data("nas1982", package = "betaSandwich")

# Fit Model in lm ----------------------------------------------------------
object <- stats::lm(
  formula = QUALITY ~ NARTIC + PCTGRT + PCTSUPP,
  data = nas1982
)

# BetaHC -------------------------------------------------------------------
std <- BetaHC(object = object)

## Methods -----------------------------------------------------------------
summary(object = std)
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
