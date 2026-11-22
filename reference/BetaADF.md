# Estimate Standardized Regression Coefficients and the Corresponding Sampling Covariance Matrix Using the Asymptotic Distribution-Free Approach

Estimate Standardized Regression Coefficients and the Corresponding
Sampling Covariance Matrix Using the Asymptotic Distribution-Free
Approach

## Usage

``` r
BetaADF(object, alpha = c(0.05, 0.01, 0.001), std_y = TRUE, std_x = NULL)
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

Note that while the calculation in `BetaADF()` is different from
[`betaDelta::BetaDelta()`](https://jeksterslab.github.io/betaDelta//reference/BetaDelta.html)
with `type = "adf"`, the results are numerically equivalent. `BetaADF()`
is appropriate when sample sizes are moderate to large (`n > 250`).
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
covariance matrix, evaluated at the estimates (Dudgeon, 2017). The
asymptotically distribution-free sampling covariance matrix is
\$\$\widehat{\operatorname{var}} \left( \hat{\boldsymbol{\theta}}^{\ast}
\right) = \frac{1}{n} \left( \boldsymbol{J}^{\prime}
\hat{\boldsymbol{\Gamma}}^{-1} \boldsymbol{J} \right)^{-1},\$\$ where
\\(\hat{\boldsymbol{\Gamma}}\\) is the unbiased estimator
\$\$\hat{\boldsymbol{\Gamma}} = \frac{n \left( n - 1 \right)}{\left( n -
2 \right) \left( n - 3 \right)}
\hat{\boldsymbol{\Gamma}}\_{\mathrm{ADF}} - \frac{n}{\left( n - 2
\right) \left( n - 3 \right)} \left(
\hat{\boldsymbol{\Gamma}}\_{\mathrm{N}} - \frac{2}{n - 1}
\operatorname{vech} \left( \tilde{\boldsymbol{S}} \right)
\operatorname{vech} \left( \tilde{\boldsymbol{S}} \right)^{\prime}
\right),\$\$ \\(\tilde{\boldsymbol{S}}\\) is the covariance matrix with
divisor \\(n\\), \\(\hat{\boldsymbol{\Gamma}}\_{\mathrm{N}}\\) is the
normal-theory matrix (see
[`BetaN()`](https://jeksterslab.github.io/betaSandwich/reference/BetaN.md))
at \\(\tilde{\boldsymbol{S}}\\), and
\$\$\hat{\boldsymbol{\Gamma}}\_{\mathrm{ADF}} = \frac{1}{n} \sum\_{i =
1}^{n} \left\[ \operatorname{vech} \left( \boldsymbol{d}\_{i}
\boldsymbol{d}\_{i}^{\prime} \right) - \operatorname{vech} \left(
\tilde{\boldsymbol{S}} \right) \right\] \left\[ \operatorname{vech}
\left( \boldsymbol{d}\_{i} \boldsymbol{d}\_{i}^{\prime} \right) -
\operatorname{vech} \left( \tilde{\boldsymbol{S}} \right)
\right\]^{\prime},\$\$ with \\(\boldsymbol{d}\_{i}\\) the deviation of
observation \\(i\\) from the sample means. The block of the standardized
slopes is returned. The confidence intervals are
\\(\hat{\beta}^{\ast}\_{j} \pm t\_{1 - \alpha / 2, n - k}
\widehat{\operatorname{se}} \left( \hat{\beta}^{\ast}\_{j} \right)\\),
where \\(k\\) is the number of regression parameters, including the
intercept.

## References

Browne, M. W. (1984). Asymptotically distribution-free methods for the
analysis of covariance structures. *British Journal of Mathematical and
Statistical Psychology*, *37*(1), 62–83.
[doi:10.1111/j.2044-8317.1984.tb00789.x](https://doi.org/10.1111/j.2044-8317.1984.tb00789.x)

Dudgeon, P. (2017). Some improvements in confidence intervals for
standardized regression coefficients. *Psychometrika*, *82*(4), 928–951.
[doi:10.1007/s11336-017-9563-z](https://doi.org/10.1007/s11336-017-9563-z)

Pesigan, I. J. A., Sun, R. W., & Cheung, S. F. (2023). betaDelta and
betaSandwich: Confidence intervals for standardized regression
coefficients in R. *Multivariate Behavioral Research*.
[doi:10.1080/00273171.2023.2201277](https://doi.org/10.1080/00273171.2023.2201277)

## See also

Other Beta Sandwich Functions:
[`BetaHC()`](https://jeksterslab.github.io/betaSandwich/reference/BetaHC.md),
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

# BetaADF ------------------------------------------------------------------
std <- BetaADF(object = object)

## Methods -----------------------------------------------------------------
print(x = std)
#> Call:
#> BetaADF(object = object)
#> 
#> Standardized regression slopes with MVN standard errors:
#>            est     se      t df      p   0.05%   0.5%   2.5%  97.5%  99.5%
#> NARTIC  0.4951 0.0674 7.3490 42 0.0000  0.2568 0.3134 0.3592 0.6311 0.6769
#> PCTGRT  0.3915 0.0710 5.5164 42 0.0000  0.1404 0.2000 0.2483 0.5347 0.5830
#> PCTSUPP 0.2632 0.0769 3.4231 42 0.0014 -0.0088 0.0558 0.1081 0.4184 0.4707
#>         99.95%
#> NARTIC  0.7335
#> PCTGRT  0.6426
#> PCTSUPP 0.5353
summary(object = std)
#> Call:
#> BetaADF(object = object)
#> 
#> Standardized regression slopes with MVN standard errors:
#>            est     se      t df      p   0.05%   0.5%   2.5%  97.5%  99.5%
#> NARTIC  0.4951 0.0674 7.3490 42 0.0000  0.2568 0.3134 0.3592 0.6311 0.6769
#> PCTGRT  0.3915 0.0710 5.5164 42 0.0000  0.1404 0.2000 0.2483 0.5347 0.5830
#> PCTSUPP 0.2632 0.0769 3.4231 42 0.0014 -0.0088 0.0558 0.1081 0.4184 0.4707
#>         99.95%
#> NARTIC  0.7335
#> PCTGRT  0.6426
#> PCTSUPP 0.5353
coef(object = std)
#>    NARTIC    PCTGRT   PCTSUPP 
#> 0.4951451 0.3914887 0.2632477 
vcov(object = std)
#>               NARTIC       PCTGRT      PCTSUPP
#> NARTIC   0.004539472 -0.002552698 -0.001742698
#> PCTGRT  -0.002552698  0.005036538 -0.001906216
#> PCTSUPP -0.001742698 -0.001906216  0.005914088
confint(object = std, level = 0.95)
#>             2.5 %    97.5 %
#> NARTIC  0.3591757 0.6311146
#> PCTGRT  0.2482683 0.5347091
#> PCTSUPP 0.1080509 0.4184444
```
