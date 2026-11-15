# betaSandwich

Ivan Jacob Agaloos Pesigan 2026-11-15

## Description

Generates robust confidence intervals for standardized regression
coefficients using heteroskedasticity-consistent standard errors for
models fitted by [`lm()`](https://rdrr.io/r/stats/lm.html) as described
in Dudgeon (2017: <http://doi.org/10.1007/s11336-017-9563-z>). Sampling
covariance matrices can also be obtained under multivariate normal
theory and the asymptotically distribution-free approach. The package
can also be used to generate confidence intervals for R-squared,
adjusted R-squared, differences of standardized regression coefficients,
semipartial correlations, squared partial correlations, the improvement
in R-squared, and Cohen’s f-squared. A description of the package and
code examples are presented in Pesigan, Sun, and Cheung (2023:
<https://doi.org/10.1080/00273171.2023.2201277>).

## Installation

You can install the CRAN release of `betaSandwich` with:

\
[`install.packages`](https://rdrr.io/r/utils/install.packages.html)`(``"betaSandwich"``)`

You can install the development version of `betaSandwich` from
[GitHub](https://github.com/jeksterslab/betaSandwich) with:

\
`if`` ``(``!`[`require`](https://rdrr.io/r/base/library.html)`(`[`"pak"`](https://pak.r-lib.org/)`)``)`` `[`install.packages`](https://rdrr.io/r/utils/install.packages.html)`(``"pak"``)`\
`pak``::`[`pkg_install`](https://pak.r-lib.org/reference/pkg_install.html)`(``"jeksterslab/betaSandwich"``)`

## Example

In this example, a multiple regression model is fitted using program
quality ratings (`QUALITY`) as the regressand/outcome variable and
number of published articles attributed to the program faculty members
(`NARTIC`), percent of faculty members holding research grants
(`PCTGRT`), and percentage of program graduates who received support
(`PCTSUPP`) as regressor/predictor variables using a data set from 1982
ratings of 46 doctoral programs in psychology in the USA (National
Research Council, 1982). Robust confidence intervals for the
standardized regression coefficients are generated using the
[`BetaHC()`](https://jeksterslab.github.io/betaSandwich/reference/BetaHC.md)
function from the `betaSandwich` package following Dudgeon (2017).

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`betaSandwich`](https://github.com/jeksterslab/betaSandwich)`)`

\
`df`` ``<-`` ``betaSandwich``::`[`nas1982`](https://jeksterslab.github.io/betaSandwich/reference/nas1982.md)

### Fit the regression model using the `lm()` function.

\
`object`` ``<-`` `[`lm`](https://rdrr.io/r/stats/lm.html)`(``QUALITY`` ``~`` ``NARTIC`` ``+`` ``PCTGRT`` ``+`` ``PCTSUPP``, data ``=`` ``df``)`

### Estimate the standardized regression slopes and the corresponding robust sampling covariance matrix.

\
[`BetaHC`](https://jeksterslab.github.io/betaSandwich/reference/BetaHC.md)`(``object``, type ``=`` ``"hc3"``, alpha ``=`` ``0.05``)`\
`#> Call:`\
`#> BetaHC(object = object, type = "hc3", alpha = 0.05)`\
`#> `\
`#> Standardized regression slopes with HC3 standard errors:`\
`#>            est     se      t df      p   2.5%  97.5%`\
`#> NARTIC  0.4951 0.0786 6.3025 42 0.0000 0.3366 0.6537`\
`#> PCTGRT  0.3915 0.0818 4.7831 42 0.0000 0.2263 0.5567`\
`#> PCTSUPP 0.2632 0.0855 3.0786 42 0.0037 0.0907 0.4358`

## Other Features

The package provides the following functions.

| Function | Purpose |
|----|----|
| [`BetaHC()`](https://jeksterslab.github.io/betaSandwich/reference/BetaHC.md) | Standardized regression coefficients with a heteroskedasticity-consistent sampling covariance matrix (HC0-HC5) |
| [`BetaADF()`](https://jeksterslab.github.io/betaSandwich/reference/BetaADF.md) | Standardized regression coefficients with an asymptotically distribution-free sampling covariance matrix |
| [`BetaN()`](https://jeksterslab.github.io/betaSandwich/reference/BetaN.md) | Standardized regression coefficients with a multivariate normal-theory sampling covariance matrix |
| [`RSqSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/RSqSandwich.md) | R-squared and adjusted R-squared |
| [`DiffBetaSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/DiffBetaSandwich.md) | Differences of standardized regression coefficients |
| [`SCorSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/SCorSandwich.md) | Semipartial correlations |
| [`SCorSqSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/SCorSqSandwich.md) | Squared semipartial correlations (squared signed interval) |
| [`PCorSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/PCorSandwich.md) | Squared partial correlations |
| [`DeltaRSqSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/DeltaRSqSandwich.md) | Improvement in R-squared |
| [`FSqSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/FSqSandwich.md) | Cohen’s f-squared and local f-squared |

The [`coef()`](https://rdrr.io/r/stats/coef.html),
[`vcov()`](https://rdrr.io/r/stats/vcov.html),
[`confint()`](https://rdrr.io/r/stats/confint.html),
[`summary()`](https://rdrr.io/r/base/summary.html), and
[`print()`](https://rdrr.io/r/base/print.html) methods are available for
the returned objects. Examples for each function are in the package
vignettes.

## Effect Sizes

Let \boldsymbol{\beta}^{\ast} be the vector of standardized slopes,
\mathbf{R}\_{x} the correlation matrix of the regressors, r^{jj} the jth
diagonal element of \mathbf{R}\_{x}^{-1}, n the sample size, and k the
number of parameters of the regression model, including the intercept.
Each effect size below is a function g\left( \boldsymbol{\theta}^{\ast}
\right) of the standardized parameters. Its sampling variance is
approximated by the delta method,

\widehat{\mathrm{var}} \left( \hat{g} \right) = \mathbf{J}\_{g}
\widehat{\boldsymbol{\Sigma}}\_{\boldsymbol{\theta}^{\ast}}
\mathbf{J}\_{g}^{\prime},

where \mathbf{J}\_{g} is the Jacobian of g and
\widehat{\boldsymbol{\Sigma}}\_{\boldsymbol{\theta}^{\ast}} is the
sampling covariance matrix from
[`BetaHC()`](https://jeksterslab.github.io/betaSandwich/reference/BetaHC.md),
[`BetaADF()`](https://jeksterslab.github.io/betaSandwich/reference/BetaADF.md),
or
[`BetaN()`](https://jeksterslab.github.io/betaSandwich/reference/BetaN.md)
(Yuan & Chan, 2011). The three functions differ in the estimator of the
asymptotic covariance matrix of the sample covariances.
[`BetaN()`](https://jeksterslab.github.io/betaSandwich/reference/BetaN.md)
assumes multivariate normality (Browne, 1984).
[`BetaADF()`](https://jeksterslab.github.io/betaSandwich/reference/BetaADF.md)
uses sample fourth moments and requires only that they exist, but it is
noisy in small samples (Browne, 1984; Jones & Waller, 2015).
[`BetaHC()`](https://jeksterslab.github.io/betaSandwich/reference/BetaHC.md)
keeps the normal-theory model for the regressors but replaces the
assumption of constant error variance with the squared residuals of the
cases (HC0 to HC5) (Cribari-Neto, 2004; Dudgeon, 2017; MacKinnon &
White, 1985; White, 1980), and it is recommended in most situations.
Confidence intervals are \hat{g} \pm z\_{1 - \alpha / 2} \\,
\widehat{\mathrm{se}} \left( \hat{g} \right).

**R-squared and adjusted R-squared**
([`RSqSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/RSqSandwich.md))

R^{2} = \boldsymbol{\beta}^{\ast\prime} \mathbf{R}\_{x}
\boldsymbol{\beta}^{\ast}, \quad \bar{R}^{2} = 1 - \left( 1 - R^{2}
\right) \frac{n - 1}{n - k}.

R^{2} is the proportion of the variance of the outcome explained by the
regressors. \bar{R}^{2} corrects R^{2} for the number of regressors.

**Semipartial correlation**
([`SCorSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/SCorSandwich.md))

sr\_{j} = \frac{\beta^{\ast}\_{j}}{\sqrt{r^{jj}}} = \beta^{\ast}\_{j}
\sqrt{1 - R^{2}\_{j \cdot -j}},

where R^{2}\_{j \cdot -j} is the R-squared from regressing regressor j
on the other regressors. It is the correlation between the outcome and
the part of regressor j that is not explained by the other regressors.
Because 1 / r^{jj} = 1 - R^{2}\_{j \cdot -j}, sr\_{j} shrinks toward
zero as regressor j becomes more redundant with the other regressors,
even when \beta^{\ast}\_{j} does not.

**Improvement in R-squared**
([`DeltaRSqSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/DeltaRSqSandwich.md))

\Delta R^{2}\_{j} = R^{2} - R^{2}\_{-j} = sr^{2}\_{j}.

It is the change in R^{2} when regressor j is added last, the proportion
of the variance of the outcome that regressor j alone accounts for.

**Squared semipartial correlation interval**
([`SCorSqSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/SCorSqSandwich.md))

The interval \left\[ l, u \right\] of the signed sr\_{j} is squared as a
set. The result is \left\[ l^{2}, u^{2} \right\] if 0 \< l, \left\[
u^{2}, l^{2} \right\] if u \< 0, and \left\[ 0, \max \left( l^{2}, u^{2}
\right) \right\] if the interval contains zero. This is not the
delta-method interval of
[`DeltaRSqSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/DeltaRSqSandwich.md).

**Squared partial correlation**
([`PCorSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/PCorSandwich.md))

pr^{2}\_{j} = \frac{sr^{2}\_{j}}{1 - R^{2} + sr^{2}\_{j}}.

It is the proportion of the variance of the outcome not explained by the
other regressors that is explained by regressor j.

**Cohen’s f-squared and local f-squared**
([`FSqSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/FSqSandwich.md))

f^{2} = \frac{R^{2}}{1 - R^{2}}, \quad f^{2}\_{j} =
\frac{sr^{2}\_{j}}{1 - R^{2}}.

By convention (Cohen, 1988), values of about 0.02, 0.15, and 0.35 are
small, medium, and large effects.

**Differences of standardized slopes**
([`DiffBetaSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/DiffBetaSandwich.md))

\beta^{\ast}\_{j} - \beta^{\ast}\_{l}.

It compares the standardized effects of regressors j and l on the same
scale.

**Effect sizes near zero**

\beta^{\ast}\_{j} and sr\_{j} are signed and have no boundary at zero.
R^{2}, \Delta R^{2}\_{j}, pr^{2}\_{j}, and f^{2}\_{j} cannot be
negative. When the population value is zero, the gradient of the squared
effect sizes is zero, the first-order delta-method variance vanishes,
and a symmetric Wald interval is not justified. Near zero, the sampling
distribution remains skewed and bounded in finite samples, and the
interval can extend below zero. An interval for sr\_{j} can therefore
behave well where the interval for sr^{2}\_{j} does not.
[`SCorSqSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/SCorSqSandwich.md)
respects the bound by squaring the interval of sr\_{j} as a set.

## Documentation

See [GitHub
Pages](https://jeksterslab.github.io/betaSandwich/index.html) for
package documentation.

## Citation

To cite `betaSandwich` in publications, please cite Pesigan et al.
(2023).

## References

Browne, M. W. (1984). Asymptotically distribution-free methods for the
analysis of covariance structures. *British Journal of Mathematical and
Statistical Psychology*, *37*(1), 62–83.
<https://doi.org/10.1111/j.2044-8317.1984.tb00789.x>

Cohen, J. (1988). *Statistical power analysis for the behavioral
sciences* (2nd ed.). Routledge. <https://doi.org/10.4324/9780203771587>

Cribari-Neto, F. (2004). Asymptotic inference under heteroskedasticity
of unknown form. *Computational Statistics & Data Analysis*, *45*(2),
215–233. <https://doi.org/10.1016/s0167-9473(02)00366-3>

Dudgeon, P. (2017). Some improvements in confidence intervals for
standardized regression coefficients. *Psychometrika*, *82*(4), 928–951.
<https://doi.org/10.1007/s11336-017-9563-z>

Jones, J. A., & Waller, N. G. (2015). The normal-theory and asymptotic
distribution-free (ADF) covariance matrix of standardized regression
coefficients: Theoretical extensions and finite sample behavior.
*Psychometrika*, *80*(2), 365–378.
<https://doi.org/10.1007/s11336-013-9380-y>

MacKinnon, J. G., & White, H. (1985). Some heteroskedasticity-consistent
covariance matrix estimators with improved finite sample properties.
*Journal of Econometrics*, *29*(3), 305–325.
<https://doi.org/10.1016/0304-4076(85)90158-7>

National Research Council. (1982). *An assessment of research-doctorate
programs in the United States: Social and behavioral sciences*. National
Academies Press. <https://doi.org/10.17226/9781>

Pesigan, I. J. A., Sun, R. W., & Cheung, S. F. (2023). betaDelta and
betaSandwich: Confidence intervals for standardized regression
coefficients in R. *Multivariate Behavioral Research*, *58*(6),
1183–1186. <https://doi.org/10.1080/00273171.2023.2201277>

R Core Team. (2026). *R: A language and environment for statistical
computing*. R Foundation for Statistical Computing.
<https://www.R-project.org/>

White, H. (1980). A heteroskedasticity-consistent covariance matrix
estimator and a direct test for heteroskedasticity. *Econometrica*,
*48*(4), 817–838. <https://doi.org/10.2307/1912934>

Yuan, K.-H., & Chan, W. (2011). Biases and standard errors of
standardized regression coefficients. *Psychometrika*, *76*(4), 670–690.
<https://doi.org/10.1007/s11336-011-9224-6>
