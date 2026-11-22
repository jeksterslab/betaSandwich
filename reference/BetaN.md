# Estimate Standardized Regression Coefficients and the Corresponding Sampling Covariance Matrix Assuming Multivariate Normality

Estimate Standardized Regression Coefficients and the Corresponding
Sampling Covariance Matrix Assuming Multivariate Normality

## Usage

``` r
BetaN(object, alpha = c(0.05, 0.01, 0.001), std_y = TRUE, std_x = NULL)
```

## Arguments

- object:

  Object of class `lm`.

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

Note that while the calculation in `BetaN()` is different from
[`betaDelta::BetaDelta()`](https://jeksterslab.github.io/betaDelta//reference/BetaDelta.html)
with `type = "mvn"`, the results are numerically equivalent. `BetaN()`
assumes multivariate normality.
[`BetaHC()`](https://jeksterslab.github.io/betaSandwich/reference/BetaHC.md)
is recommended in most situations.

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
covariance matrix, evaluated at the estimates (Dudgeon, 2017). Under
multivariate normality, \$\$\widehat{\operatorname{var}} \left(
\hat{\boldsymbol{\theta}}^{\ast} \right) = \frac{1}{n} \left(
\boldsymbol{J}^{\prime} \hat{\boldsymbol{\Gamma}}\_{\mathrm{N}}^{-1}
\boldsymbol{J} \right)^{-1}, \quad
\hat{\boldsymbol{\Gamma}}\_{\mathrm{N}} = 2 \boldsymbol{D}^{+} \left(
\boldsymbol{S} \otimes \boldsymbol{S} \right) \boldsymbol{D}^{+
\prime},\$\$ where \\(\boldsymbol{S}\\) is the sample covariance matrix
and \\(\boldsymbol{D}^{+}\\) is the Moore-Penrose inverse of the
duplication matrix. The block of the standardized slopes is returned.
The confidence intervals are \\(\hat{\beta}^{\ast}\_{j} \pm t\_{1 -
\alpha / 2, n - k} \widehat{\operatorname{se}} \left(
\hat{\beta}^{\ast}\_{j} \right)\\), where \\(k\\) is the number of
regression parameters, including the intercept.

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
[`BetaHC()`](https://jeksterslab.github.io/betaSandwich/reference/BetaHC.md),
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

# BetaN --------------------------------------------------------------------
std <- BetaN(object = object)

## Methods -----------------------------------------------------------------
print(x = std)
#> Call:
#> BetaN(object = object)
#> 
#> Standardized regression slopes with MVN standard errors:
#>            est     se      t df     p   0.05%   0.5%   2.5%  97.5%  99.5%
#> NARTIC  0.4951 0.0759 6.5272 42 0.000  0.2268 0.2905 0.3421 0.6482 0.6998
#> PCTGRT  0.3915 0.0770 5.0824 42 0.000  0.1190 0.1837 0.2360 0.5469 0.5993
#> PCTSUPP 0.2632 0.0747 3.5224 42 0.001 -0.0011 0.0616 0.1124 0.4141 0.4649
#>         99.95%
#> NARTIC  0.7635
#> PCTGRT  0.6640
#> PCTSUPP 0.5276
summary(object = std)
#> Call:
#> BetaN(object = object)
#> 
#> Standardized regression slopes with MVN standard errors:
#>            est     se      t df     p   0.05%   0.5%   2.5%  97.5%  99.5%
#> NARTIC  0.4951 0.0759 6.5272 42 0.000  0.2268 0.2905 0.3421 0.6482 0.6998
#> PCTGRT  0.3915 0.0770 5.0824 42 0.000  0.1190 0.1837 0.2360 0.5469 0.5993
#> PCTSUPP 0.2632 0.0747 3.5224 42 0.001 -0.0011 0.0616 0.1124 0.4141 0.4649
#>         99.95%
#> NARTIC  0.7635
#> PCTGRT  0.6640
#> PCTSUPP 0.5276
coef(object = std)
#>    NARTIC    PCTGRT   PCTSUPP 
#> 0.4951451 0.3914887 0.2632477 
vcov(object = std)
#>               NARTIC       PCTGRT      PCTSUPP
#> NARTIC   0.005754524 -0.003360334 -0.002166127
#> PCTGRT  -0.003360334  0.005933462 -0.001769723
#> PCTSUPP -0.002166127 -0.001769723  0.005585256
confint(object = std, level = 0.95)
#>             2.5 %    97.5 %
#> NARTIC  0.3420563 0.6482339
#> PCTGRT  0.2360380 0.5469395
#> PCTSUPP 0.1124272 0.4140682
```
