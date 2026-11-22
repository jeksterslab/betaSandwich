# Confidence Intervals for an Object of Class `essandwich`

Confidence Intervals for an Object of Class `essandwich`

## Usage

``` r
# S3 method for class 'essandwich'
confint(object, parm = NULL, level = 0.95, ...)
```

## Arguments

- object:

  Object of class `essandwich`.

- parm:

  a specification of which parameters are to be given confidence
  intervals, either a vector of numbers or a vector of names. If
  missing, all parameters are considered.

- level:

  the confidence level required.

- ...:

  additional arguments.

## Value

Returns a matrix of confidence intervals.

## Details

The confidence intervals are \\(\hat{\theta} \pm z\_{1 - \alpha / 2}
\widehat{\operatorname{se}} \left( \hat{\theta} \right)\\), where
\\(\widehat{\operatorname{se}}\\) is the square root of the diagonal of
the delta-method sampling covariance matrix of the effect size, and
\\(z\_{p}\\) is the \\(p\\) quantile of the standard normal
distribution. The test statistic is \\(z = \hat{\theta} /
\widehat{\operatorname{se}} \left( \hat{\theta} \right)\\).

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

# RSqSandwich --------------------------------------------------------------
out <- RSqSandwich(object = std)

## Methods -----------------------------------------------------------------
confint(object = out, level = 0.95)
#>         2.5 %    97.5 %
#> rsq 0.6921245 0.9169280
#> adj 0.6701334 0.9109943
```
