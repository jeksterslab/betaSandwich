# Estimate Standardized Regression Coefficients and the Corresponding Robust Sampling Covariance Matrix Using the Heteroskedasticity Consistent Approach

Estimate Standardized Regression Coefficients and the Corresponding
Robust Sampling Covariance Matrix Using the Heteroskedasticity
Consistent Approach

## Usage

``` r
BetaHC(
  object,
  type = "hc3",
  alpha = c(0.05, 0.01, 0.001),
  std_y = TRUE,
  std_x = NULL,
  g1 = 1,
  g2 = 1.5,
  k = 0.7
)
```

## Arguments

- object:

  Object of class `lm`.

- type:

  Character string. Correction type. Possible values are `"hc0"`,
  `"hc1"`, `"hc2"`, `"hc3"`, `"hc4"`, `"hc4m"`, and `"hc5"`.

- alpha:

  Numeric vector. Significance level \\(\alpha\\).

- std_y:

  Logical. Standardize \\(y\\). With `std_y = FALSE`, \\(y\\) is left in
  its original units (\\(b_j \sigma\_{x_j}\\)).

- std_x:

  Character vector or `NULL`. Names of the regressors to standardize.
  `NULL` (default) standardizes all regressors; `character(0)`
  standardizes none (\\(b_j / \sigma_y\\)). Leaving a dummy variable out
  keeps its 0-1 contrast. The delta method accounts for the sampling
  variability of the standard deviations.

- g1:

  Numeric. `g1` value for `type = "hc4m"`.

- g2:

  Numeric. `g2` value for `type = "hc4m"`.

- k:

  Numeric. Constant `k` for `type = "hc5"` \\(0 \leq k \leq 1\\).

## Value

Returns an object of class `betasandwich` which is a list with the
following elements:

- call:

  Function call.

- args:

  Function arguments.

- lm_process:

  Processed `lm` object.

- gamma_n:

  Asymptotic covariance matrix of the sample covariance matrix assuming
  multivariate normality.

- gamma_hc:

  Asymptotic covariance matrix HC correction.

- gamma:

  Asymptotic covariance matrix of the sample covariance matrix.

- acov:

  Asymptotic covariance matrix of the standardized slopes.

- vcov:

  Sampling covariance matrix of the standardized slopes.

- est:

  Vector of standardized slopes.

## Details

Partition the covariance matrix of the outcome and the regressors as
\$\$\boldsymbol{\Sigma} = \begin{pmatrix} \sigma^{2}\_{y} &
\boldsymbol{\sigma}\_{yx}^{\prime} \\\\ \boldsymbol{\sigma}\_{yx} &
\boldsymbol{\Sigma}\_{x} \end{pmatrix}.\$\$ The regression slopes and
the standardized regression slopes are \$\$\boldsymbol{\beta} =
\boldsymbol{\Sigma}\_{x}^{-1} \boldsymbol{\sigma}\_{yx}, \quad
\beta^{\ast}\_{j} = \beta\_{j} \frac{\sigma\_{x\_{j}}}{\sigma\_{y}},\$\$
where \\(\sigma\_{x\_{j}}\\) is the standard deviation of regressor
\\(j\\). With `std_y = FALSE`, \\(\sigma\_{y}\\) is replaced by one; for
a regressor left out of `std_x`, \\(\sigma\_{x\_{j}}\\) is replaced by
one.

Let \\(\boldsymbol{\theta}^{\ast}\\) collect the standardized slopes,
\\(\sigma\_{y}\\), the standard deviations of the regressors, and the
correlations of the regressors, and let \\(\boldsymbol{J} = \partial
\operatorname{vech} \left( \boldsymbol{\Sigma} \right) / \partial
\boldsymbol{\theta}^{\ast \prime}\\) be the Jacobian of the implied
covariance matrix, evaluated at the estimates (Dudgeon, 2017). The
heteroskedasticity-consistent sampling covariance matrix is the sandwich
\$\$\widehat{\operatorname{var}} \left( \hat{\boldsymbol{\theta}}^{\ast}
\right) = \frac{n}{\left( n - 1 \right)^{2}} \boldsymbol{B}^{-1}
\boldsymbol{J}^{\prime} \hat{\boldsymbol{\Gamma}}\_{\mathrm{N}}^{-1}
\hat{\boldsymbol{\Gamma}}\_{\mathrm{HC}}
\hat{\boldsymbol{\Gamma}}\_{\mathrm{N}}^{-1} \boldsymbol{J}
\boldsymbol{B}^{-1}, \quad \boldsymbol{B} = \boldsymbol{J}^{\prime}
\hat{\boldsymbol{\Gamma}}\_{\mathrm{N}}^{-1} \boldsymbol{J},\$\$ where
\\(\hat{\boldsymbol{\Gamma}}\_{\mathrm{N}}\\) is the normal-theory
matrix (see
[`BetaN()`](https://jeksterslab.github.io/betaSandwich/reference/BetaN.md))
and \$\$\hat{\boldsymbol{\Gamma}}\_{\mathrm{HC}} = \frac{1}{n} \sum\_{i
= 1}^{n} q\_{i} \boldsymbol{v}\_{i} \boldsymbol{v}\_{i}^{\prime}, \quad
\boldsymbol{v}\_{i} = \operatorname{vech} \left( \boldsymbol{d}\_{i}
\boldsymbol{d}\_{i}^{\prime} \right) - \operatorname{vech} \left(
\boldsymbol{S} \right),\$\$ with \\(\boldsymbol{d}\_{i}\\) the deviation
of observation \\(i\\) from the sample means. The weight \\(q\_{i}\\)
depends on the leverage \\(h\_{i}\\), the number of regression
parameters \\(k\\) (including the intercept), and `type`: \\(q\_{i} =
1\\) (`"hc0"`, `"hc1"`), \\(\left( 1 - h\_{i} \right)^{-1}\\) (`"hc2"`),
\\(\left( 1 - h\_{i} \right)^{-2}\\) (`"hc3"`), \\(\left( 1 - h\_{i}
\right)^{-\delta\_{i}}\\) with \\(\delta\_{i} = \min \left( 4, n h\_{i}
/ k \right)\\) (`"hc4"`), \\(\left( 1 - h\_{i}
\right)^{-\lambda\_{i}}\\) with \\(\lambda\_{i} = \min \left( g\_{1}, n
h\_{i} / k \right) + \min \left( g\_{2}, n h\_{i} / k \right)\\)
(`"hc4m"`), and \\(\left( 1 - h\_{i} \right)^{-\gamma\_{i} / 2}\\) with
\\(\gamma\_{i} = \min \left( n h\_{i} / k, \max \left( 4, n c \max\_{l}
h\_{l} / k \right) \right)\\) (`"hc5"`, where \\(c\\) is the argument
`k`). With `"hc1"`, the matrix is multiplied by \\(n / \left( n - k
\right)\\). The block of the standardized slopes is returned. The
confidence intervals are \\(\hat{\beta}^{\ast}\_{j} \pm t\_{1 - \alpha /
2, n - k} \widehat{\operatorname{se}} \left( \hat{\beta}^{\ast}\_{j}
\right)\\), where \\(k\\) is the number of regression parameters,
including the intercept.

## References

Dudgeon, P. (2017). Some improvements in confidence intervals for
standardized regression coefficients. *Psychometrika*, *82*(4), 928–951.
[doi:10.1007/s11336-017-9563-z](https://doi.org/10.1007/s11336-017-9563-z)

Pesigan, I. J. A., Sun, R. W., & Cheung, S. F. (2023). betaDelta and
betaSandwich: Confidence intervals for standardized regression
coefficients in R. *Multivariate Behavioral Research*.
[doi:10.1080/00273171.2023.2201277](https://doi.org/10.1080/00273171.2023.2201277)

## See also

Other Beta Sandwich Functions:
[`BetaADF()`](https://jeksterslab.github.io/betaSandwich/reference/BetaADF.md),
[`BetaN()`](https://jeksterslab.github.io/betaSandwich/reference/BetaN.md),
[`DeltaRSqSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/DeltaRSqSandwich.md),
[`DiffBetaSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/DiffBetaSandwich.md),
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

## Methods -----------------------------------------------------------------
print(x = std)
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
coef(object = std)
#>    NARTIC    PCTGRT   PCTSUPP 
#> 0.4951451 0.3914887 0.2632477 
vcov(object = std)
#>               NARTIC       PCTGRT      PCTSUPP
#> NARTIC   0.006172168 -0.003602529 -0.001943469
#> PCTGRT  -0.003602529  0.006699155 -0.002443584
#> PCTSUPP -0.001943469 -0.002443584  0.007311625
confint(object = std, level = 0.95)
#>              2.5 %    97.5 %
#> NARTIC  0.33659828 0.6536920
#> PCTGRT  0.22631203 0.5566654
#> PCTSUPP 0.09068548 0.4358099
```
