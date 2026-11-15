# betaSandwich: Example Using the SCorSandwich, SCorSqSandwich, PCorSandwich, DeltaRSqSandwich, and FSqSandwich Functions

Confidence intervals for semipartial correlations, squared semipartial
correlations, squared partial correlations, the improvement in
R-squared, and Cohen’s f-squared are generated using the
[`SCorSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/SCorSandwich.md),
[`SCorSqSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/SCorSqSandwich.md),
[`PCorSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/PCorSandwich.md),
[`DeltaRSqSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/DeltaRSqSandwich.md),
and
[`FSqSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/FSqSandwich.md)
functions from the `betaSandwich` package. In this example, we use the
data set and the model used in [betaSandwich: Example Using the BetaHC
Function](https://jeksterslab.github.io/betaSandwich/articles/example-beta-sandwich.md).

\
[`library`](https://rdrr.io/r/base/library.html)`(``package ``=`` ``betaSandwich``)`

\
`df`` ``<-`` ``betaSandwich``::`[`nas1982`](https://jeksterslab.github.io/betaSandwich/reference/nas1982.md)

## Effect sizes

Let \boldsymbol{\beta}^{\ast} be the vector of standardized slopes,
\boldsymbol{R}\_{x} the correlation matrix of the regressors, r^{jj} the
jth diagonal element of \boldsymbol{R}\_{x}^{-1}, n the sample size, and
k the number of parameters of the regression model, including the
intercept. Each effect size below is a function g\left(
\boldsymbol{\theta}^{\ast} \right) of the standardized parameters. Its
sampling variance is approximated by the delta method,

\widehat{\operatorname{var}} \left( \hat{g} \right) =
\boldsymbol{J}\_{g}
\widehat{\boldsymbol{\Sigma}}\_{\boldsymbol{\theta}^{\ast}}
\boldsymbol{J}\_{g}^{\prime},

where \boldsymbol{J}\_{g} is the Jacobian of g and
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

R^{2} = \boldsymbol{\beta}^{\ast\prime} \boldsymbol{R}\_{x}
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

## Fit the regression model using the `lm()` function.

\
`object`` ``<-`` ``stats``::`[`lm`](https://rdrr.io/r/stats/lm.html)`(``formula ``=`` ``QUALITY`` ``~`` ``NARTIC`` ``+`` ``PCTGRT`` ``+`` ``PCTSUPP``, data ``=`` ``df``)`

## Estimate the standardized regression slopes and the corresponding sampling covariance matrix.

### Multivariate Normal-Theory Approach

\
`std_mvn`` ``<-`` `[`BetaN`](https://jeksterslab.github.io/betaSandwich/reference/BetaN.md)`(``object ``=`` ``object``)`

#### Asymptotic Distribution-Free Approach

\
`std_adf`` ``<-`` `[`BetaADF`](https://jeksterslab.github.io/betaSandwich/reference/BetaADF.md)`(``object ``=`` ``object``)`

#### HC3

\
`std_hc3`` ``<-`` `[`BetaHC`](https://jeksterslab.github.io/betaSandwich/reference/BetaHC.md)`(``object ``=`` ``object``, type ``=`` ``"hc3"``)`

## Semipartial correlations

The semipartial correlation of regressor j is sr\_{j} =
\beta^{\ast}\_{j} / \sqrt{\left\[ \boldsymbol{R}\_{x}^{-1}
\right\]\_{jj}}, the correlation between the outcome and the part of
regressor j that is not explained by the other regressors.

\
[`summary`](https://rdrr.io/r/base/summary.html)`(``object ``=`` `[`SCorSandwich`](https://jeksterslab.github.io/betaSandwich/reference/SCorSandwich.md)`(``object ``=`` ``std_hc3``, alpha ``=`` ``0.05``)``)`\
`#> Call:`\
`#> SCorSandwich(object = std_hc3, alpha = 0.05)`\
`#> `\
`#> Semipartial correlations with HC3 standard errors:`\
`#>            est     se      z      p   2.5%  97.5%`\
`#> NARTIC  0.4312 0.0794 5.4303 0.0000 0.2755 0.5868`\
`#> PCTGRT  0.3430 0.0789 4.3468 0.0000 0.1883 0.4977`\
`#> PCTSUPP 0.2385 0.0783 3.0465 0.0023 0.0851 0.3919`

## Squared semipartial correlations

The
[`SCorSqSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/SCorSqSandwich.md)
function squares the signed-semipartial interval as a set. If the signed
interval contains zero, the lower limit of the squared interval is zero.
Writing the signed interval as \left\[ l, u \right\], the squared
interval is \left\[ l^{2}, u^{2} \right\] if 0 \< l, \left\[ u^{2},
l^{2} \right\] if u \< 0, and \left\[ 0, \max \left( l^{2}, u^{2}
\right) \right\] otherwise. The function returns a data frame with the
estimate and the confidence limits.

\
[`SCorSqSandwich`](https://jeksterslab.github.io/betaSandwich/reference/SCorSqSandwich.md)`(``object ``=`` ``std_hc3``, level ``=`` ``0.95``)`\
`#>               est       lower     upper`\
`#> NARTIC  0.1858925 0.075919679 0.3442987`\
`#> PCTGRT  0.1176542 0.035474114 0.2476748`\
`#> PCTSUPP 0.0568722 0.007234428 0.1535875`

## Squared partial correlations

The squared partial correlation is pr^{2}\_{j} = sr^{2}\_{j} / \left(
1 - R^{2} + sr^{2}\_{j} \right), the proportion of the variance of the
outcome not explained by the other regressors that is explained by
regressor j.

\
[`summary`](https://rdrr.io/r/base/summary.html)`(``object ``=`` `[`PCorSandwich`](https://jeksterslab.github.io/betaSandwich/reference/PCorSandwich.md)`(``object ``=`` ``std_hc3``, alpha ``=`` ``0.05``)``)`\
`#> Call:`\
`#> PCorSandwich(object = std_hc3, alpha = 0.05)`\
`#> `\
`#> Squared partial correlations with HC3 standard errors:`\
`#>            est     se      z      p    2.5%  97.5%`\
`#> NARTIC  0.4874 0.1095 4.4532 0.0000  0.2729 0.7020`\
`#> PCTGRT  0.3757 0.1156 3.2500 0.0012  0.1491 0.6023`\
`#> PCTSUPP 0.2254 0.1277 1.7647 0.0776 -0.0249 0.4757`

## Improvement in R-squared

The improvement in R-squared is \Delta R^{2}\_{j} = R^{2} - R^{2}\_{-j}
= sr^{2}\_{j}, the decrease in R^{2} when regressor j is removed from
the model.

\
[`summary`](https://rdrr.io/r/base/summary.html)`(``object ``=`` `[`DeltaRSqSandwich`](https://jeksterslab.github.io/betaSandwich/reference/DeltaRSqSandwich.md)`(``object ``=`` ``std_hc3``, alpha ``=`` ``0.05``)``)`\
`#> Call:`\
`#> DeltaRSqSandwich(object = std_hc3, alpha = 0.05)`\
`#> `\
`#> Improvement in R-squared with HC3 standard errors:`\
`#>            est     se      z      p    2.5%  97.5%`\
`#> NARTIC  0.1859 0.0685 2.7151 0.0066  0.0517 0.3201`\
`#> PCTGRT  0.1177 0.0541 2.1734 0.0298  0.0116 0.2238`\
`#> PCTSUPP 0.0569 0.0373 1.5233 0.1277 -0.0163 0.1300`

## Cohen’s f-squared and local f-squared

Cohen’s f-squared is f^{2} = R^{2} / \left( 1 - R^{2} \right) and the
local f-squared of regressor j is f^{2}\_{j} = sr^{2}\_{j} / \left( 1 -
R^{2} \right). By convention, values of about 0.02, 0.15, and 0.35 are
small, medium, and large effects.

\
[`summary`](https://rdrr.io/r/base/summary.html)`(``object ``=`` `[`FSqSandwich`](https://jeksterslab.github.io/betaSandwich/reference/FSqSandwich.md)`(``object ``=`` ``std_hc3``, alpha ``=`` ``0.05``)``)`\
`#> Call:`\
`#> FSqSandwich(object = std_hc3, alpha = 0.05)`\
`#> `\
`#> Cohen's f-squared (fsq) and local f-squared with HC3 standard errors:`\
`#>            est     se      z      p    2.5%  97.5%`\
`#> fsq     4.1158 1.5009 2.7422 0.0061  1.1741 7.0575`\
`#> NARTIC  0.9510 0.4166 2.2825 0.0225  0.1344 1.7676`\
`#> PCTGRT  0.6019 0.2967 2.0288 0.0425  0.0204 1.1834`\
`#> PCTSUPP 0.2909 0.2128 1.3670 0.1716 -0.1262 0.7081`

## Extracting estimates, sampling covariance matrices, and confidence intervals

The [`coef()`](https://rdrr.io/r/stats/coef.html),
[`vcov()`](https://rdrr.io/r/stats/vcov.html), and
[`confint()`](https://rdrr.io/r/stats/confint.html) methods work on the
objects returned by the effect size functions.

\
`scor`` ``<-`` `[`SCorSandwich`](https://jeksterslab.github.io/betaSandwich/reference/SCorSandwich.md)`(``object ``=`` ``std_hc3``)`\
`stats``::`[`coef`](https://rdrr.io/r/stats/coef.html)`(``object ``=`` ``scor``)`\
`#>    NARTIC    PCTGRT   PCTSUPP `\
`#> 0.4311525 0.3430075 0.2384789`\
`stats``::`[`vcov`](https://rdrr.io/r/stats/vcov.html)`(``object ``=`` ``scor``)`\
`#>                NARTIC        PCTGRT       PCTSUPP`\
`#> NARTIC   0.0063040434 -0.0002532449 -0.0004374935`\
`#> PCTGRT  -0.0002532449  0.0062268718 -0.0010139363`\
`#> PCTSUPP -0.0004374935 -0.0010139363  0.0061275595`\
`stats``::`[`confint`](https://rdrr.io/r/stats/confint.html)`(``object ``=`` ``scor``, level ``=`` ``0.95``)`\
`#>              2.5 %    97.5 %`\
`#> NARTIC  0.27553526 0.5867697`\
`#> PCTGRT  0.18834573 0.4976693`\
`#> PCTSUPP 0.08505544 0.3919024`

## Comparing covariance estimators

\
`stats``::`[`confint`](https://rdrr.io/r/stats/confint.html)`(``object ``=`` `[`DeltaRSqSandwich`](https://jeksterslab.github.io/betaSandwich/reference/DeltaRSqSandwich.md)`(``object ``=`` ``std_mvn``)``, level ``=`` ``0.95``)`\
`#>                2.5 %    97.5 %`\
`#> NARTIC   0.055421734 0.3163632`\
`#> PCTGRT   0.017881671 0.2174267`\
`#> PCTSUPP -0.008779331 0.1225237`\
`stats``::`[`confint`](https://rdrr.io/r/stats/confint.html)`(``object ``=`` `[`DeltaRSqSandwich`](https://jeksterslab.github.io/betaSandwich/reference/DeltaRSqSandwich.md)`(``object ``=`` ``std_adf``)``, level ``=`` ``0.95``)`\
`#>                2.5 %    97.5 %`\
`#> NARTIC   0.072394224 0.2993907`\
`#> PCTGRT   0.025067308 0.2102410`\
`#> PCTSUPP -0.008691312 0.1224357`\
`stats``::`[`confint`](https://rdrr.io/r/stats/confint.html)`(``object ``=`` `[`DeltaRSqSandwich`](https://jeksterslab.github.io/betaSandwich/reference/DeltaRSqSandwich.md)`(``object ``=`` ``std_hc3``)``, level ``=`` ``0.95``)`\
`#>               2.5 %    97.5 %`\
`#> NARTIC   0.05170296 0.3200820`\
`#> PCTGRT   0.01155384 0.2237545`\
`#> PCTSUPP -0.01630434 0.1300487`

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

White, H. (1980). A heteroskedasticity-consistent covariance matrix
estimator and a direct test for heteroskedasticity. *Econometrica*,
*48*(4), 817–838. <https://doi.org/10.2307/1912934>

Yuan, K.-H., & Chan, W. (2011). Biases and standard errors of
standardized regression coefficients. *Psychometrika*, *76*(4), 670–690.
<https://doi.org/10.1007/s11336-011-9224-6>
