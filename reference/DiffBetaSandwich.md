# Estimate Differences of Standardized Regression Coefficients and the Corresponding Sampling Covariance Matrix

Estimate Differences of Standardized Regression Coefficients and the
Corresponding Sampling Covariance Matrix

## Usage

``` r
DiffBetaSandwich(object, alpha = c(0.05, 0.01, 0.001))
```

## Arguments

- object:

  Object of class `betasandwich`, that is, the output of the
  [`BetaHC()`](https://jeksterslab.github.io/betaSandwich/reference/BetaHC.md),
  [`BetaN()`](https://jeksterslab.github.io/betaSandwich/reference/BetaN.md),
  or
  [`BetaADF()`](https://jeksterslab.github.io/betaSandwich/reference/BetaADF.md)
  functions.

- alpha:

  Numeric vector. Significance level \\(\alpha\\).

## Value

Returns an object of class `essandwich` (and `diffbetasandwich`) which
is a list with the following elements:

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

- fun:

  Function used ("DiffBetaSandwich").

## Details

For every pair of regressors \\(j \< l\\), the difference of the
standardized regression slopes is \$\$\Delta \beta^{\ast}\_{jl} =
\beta^{\ast}\_{j} - \beta^{\ast}\_{l}.\$\$ Collect the differences as
\\(\boldsymbol{C} \boldsymbol{\beta}^{\ast}\\), where each row of
\\(\boldsymbol{C}\\) has \\(1\\) in column \\(j\\), \\(-1\\) in column
\\(l\\), and zeros elsewhere. The sampling covariance matrix is
\$\$\widehat{\operatorname{var}} \left( \boldsymbol{C}
\hat{\boldsymbol{\beta}}^{\ast} \right) = \boldsymbol{C}
\widehat{\operatorname{var}} \left( \hat{\boldsymbol{\beta}}^{\ast}
\right) \boldsymbol{C}^{\prime},\$\$ with
\\(\widehat{\operatorname{var}} \left( \hat{\boldsymbol{\beta}}^{\ast}
\right)\\) from
[`BetaHC()`](https://jeksterslab.github.io/betaSandwich/reference/BetaHC.md),
[`BetaN()`](https://jeksterslab.github.io/betaSandwich/reference/BetaN.md),
or
[`BetaADF()`](https://jeksterslab.github.io/betaSandwich/reference/BetaADF.md),
and the confidence intervals are \\(\Delta \hat{\beta}^{\ast}\_{jl} \pm
z\_{1 - \alpha / 2} \widehat{\operatorname{se}} \left( \Delta
\hat{\beta}^{\ast}\_{jl} \right)\\).

## See also

Other Beta Sandwich Functions:
[`BetaADF()`](https://jeksterslab.github.io/betaSandwich/reference/BetaADF.md),
[`BetaHC()`](https://jeksterslab.github.io/betaSandwich/reference/BetaHC.md),
[`BetaN()`](https://jeksterslab.github.io/betaSandwich/reference/BetaN.md),
[`DeltaRSqSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/DeltaRSqSandwich.md),
[`FSqSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/FSqSandwich.md),
[`PCorSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/PCorSandwich.md),
[`RSqSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/RSqSandwich.md),
[`SCorSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/SCorSandwich.md),
[`SCorSqSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/SCorSqSandwich.md)

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

# DiffBetaSandwich ---------------------------------------------------------
diff <- DiffBetaSandwich(object = std)

## Methods -----------------------------------------------------------------
print(x = diff)
#> Call:
#> DiffBetaSandwich(object = std)
#> 
#> Differences of standardized regression slopes with HC3 standard errors:
#>                   est     se      z      p   0.05%    0.5%    2.5%  97.5%
#> NARTIC-PCTGRT  0.1037 0.1417 0.7316 0.4644 -0.3626 -0.2613 -0.1741 0.3814
#> NARTIC-PCTSUPP 0.2319 0.1318 1.7595 0.0785 -0.2018 -0.1076 -0.0264 0.4902
#> PCTGRT-PCTSUPP 0.1282 0.1375 0.9329 0.3509 -0.3241 -0.2259 -0.1412 0.3977
#>                 99.5% 99.95%
#> NARTIC-PCTGRT  0.4686 0.5699
#> NARTIC-PCTSUPP 0.5714 0.6656
#> PCTGRT-PCTSUPP 0.4823 0.5806
summary(object = diff)
#> Call:
#> DiffBetaSandwich(object = std)
#> 
#> Differences of standardized regression slopes with HC3 standard errors:
#>                   est     se      z      p   0.05%    0.5%    2.5%  97.5%
#> NARTIC-PCTGRT  0.1037 0.1417 0.7316 0.4644 -0.3626 -0.2613 -0.1741 0.3814
#> NARTIC-PCTSUPP 0.2319 0.1318 1.7595 0.0785 -0.2018 -0.1076 -0.0264 0.4902
#> PCTGRT-PCTSUPP 0.1282 0.1375 0.9329 0.3509 -0.3241 -0.2259 -0.1412 0.3977
#>                 99.5% 99.95%
#> NARTIC-PCTGRT  0.4686 0.5699
#> NARTIC-PCTSUPP 0.5714 0.6656
#> PCTGRT-PCTSUPP 0.4823 0.5806
coef(object = diff)
#>  NARTIC-PCTGRT NARTIC-PCTSUPP PCTGRT-PCTSUPP 
#>      0.1036564      0.2318974      0.1282410 
vcov(object = diff)
#>                NARTIC-PCTGRT NARTIC-PCTSUPP PCTGRT-PCTSUPP
#> NARTIC-PCTGRT    0.020076382    0.009274583   -0.010801799
#> NARTIC-PCTSUPP   0.009274583    0.017370731    0.008096148
#> PCTGRT-PCTSUPP  -0.010801799    0.008096148    0.018897947
confint(object = diff, level = 0.95)
#>                      2.5 %    97.5 %
#> NARTIC-PCTGRT  -0.17405314 0.3813660
#> NARTIC-PCTSUPP -0.02642203 0.4902169
#> PCTGRT-PCTSUPP -0.14119483 0.3976769
```
