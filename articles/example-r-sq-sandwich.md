# betaSandwich: Example Using the RSqSandwich Function

Confidence intervals for multiple correlation are generated using the
[`RSqSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/RSqSandwich.md)
function from the `betaSandwich` package. In this example, we use the
data set and the model used in [betaSandwich: Example Using the BetaHC
Function](https://jeksterslab.github.io/betaSandwich/articles/example-beta-sandwich.md).

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`betaSandwich`](https://github.com/jeksterslab/betaSandwich)`)`

\
`df`` ``<-`` ``betaSandwich``::`[`nas1982`](https://jeksterslab.github.io/betaSandwich/reference/nas1982.md)

## Fit the regression model using the `lm()` function.

\
`object`` ``<-`` `[`lm`](https://rdrr.io/r/stats/lm.html)`(``QUALITY`` ``~`` ``NARTIC`` ``+`` ``PCTGRT`` ``+`` ``PCTSUPP``, data ``=`` ``df``)`

## Estimate the standardized regression slopes and the corresponding sampling covariance matrix.

#### Multivariate Normal-Theory Approach

\
`std_mvn`` ``<-`` `[`BetaN`](https://jeksterslab.github.io/betaSandwich/reference/BetaN.md)`(``object``)`

#### Asymptotic Distribution-Free Approach

\
`std_adf`` ``<-`` `[`BetaADF`](https://jeksterslab.github.io/betaSandwich/reference/BetaADF.md)`(``object``)`

#### HC3

\
`std_hc3`` ``<-`` `[`BetaHC`](https://jeksterslab.github.io/betaSandwich/reference/BetaHC.md)`(``object``, type ``=`` ``"hc3"``)`

## Estimate the multiple correlation coefficients (R-squared and adjusted R-squared) and the corresponding sampling covariance matrix.

\
`mvn`` ``<-`` `[`RSqSandwich`](https://jeksterslab.github.io/betaSandwich/reference/RSqSandwich.md)`(``std_mvn``, alpha ``=`` ``0.05``)`\
`adf`` ``<-`` `[`RSqSandwich`](https://jeksterslab.github.io/betaSandwich/reference/RSqSandwich.md)`(``std_adf``, alpha ``=`` ``0.05``)`\
`hc3`` ``<-`` `[`RSqSandwich`](https://jeksterslab.github.io/betaSandwich/reference/RSqSandwich.md)`(``std_hc3``, alpha ``=`` ``0.05``)`

## Methods

### summary

Summary of the results of
[`RSqSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/RSqSandwich.md).

\
[`summary`](https://rdrr.io/r/base/summary.html)`(``mvn``)`\
`#> Call:`\
`#> RSqSandwich(object = std_mvn, alpha = 0.05)`\
`#> `\
`#> R-squared and adjusted R-squared with MVN standard errors:`\
`#>        est     se       z p   2.5%  97.5%`\
`#> rsq 0.8045 0.0517 15.5608 0 0.7032 0.9059`\
`#> adj 0.7906 0.0554 14.2713 0 0.6820 0.8991`\
[`summary`](https://rdrr.io/r/base/summary.html)`(``adf``)`\
`#> Call:`\
`#> RSqSandwich(object = std_adf, alpha = 0.05)`\
`#> `\
`#> R-squared and adjusted R-squared with MVN standard errors:`\
`#>        est     se       z p   2.5%  97.5%`\
`#> rsq 0.8045 0.0519 15.4910 0 0.7027 0.9063`\
`#> adj 0.7906 0.0556 14.2073 0 0.6815 0.8996`\
[`summary`](https://rdrr.io/r/base/summary.html)`(``hc3``)`\
`#> Call:`\
`#> RSqSandwich(object = std_hc3, alpha = 0.05)`\
`#> `\
`#> R-squared and adjusted R-squared with HC3 standard errors:`\
`#>        est     se       z p   2.5%  97.5%`\
`#> rsq 0.8045 0.0573 14.0286 0 0.6921 0.9169`\
`#> adj 0.7906 0.0614 12.8662 0 0.6701 0.9110`

### coef

Calculate R-squared and adjusted R-squared.

\
[`coef`](https://rdrr.io/r/stats/coef.html)`(``mvn``)`\
`#>       rsq       adj `\
`#> 0.8045263 0.7905638`\
[`coef`](https://rdrr.io/r/stats/coef.html)`(``adf``)`\
`#>       rsq       adj `\
`#> 0.8045263 0.7905638`\
[`coef`](https://rdrr.io/r/stats/coef.html)`(``hc3``)`\
`#>       rsq       adj `\
`#> 0.8045263 0.7905638`

### vcov

Calculate the sampling covariance matrix of R-squared and adjusted
R-squared.

\
[`vcov`](https://rdrr.io/r/stats/vcov.html)`(``mvn``)`\
`#>             rsq         adj`\
`#> rsq 0.002673125 0.002864062`\
`#> adj 0.002864062 0.003068638`\
[`vcov`](https://rdrr.io/r/stats/vcov.html)`(``adf``)`\
`#>             rsq         adj`\
`#> rsq 0.002697266 0.002889927`\
`#> adj 0.002889927 0.003096351`\
[`vcov`](https://rdrr.io/r/stats/vcov.html)`(``hc3``)`\
`#>             rsq         adj`\
`#> rsq 0.003288894 0.003523815`\
`#> adj 0.003523815 0.003775516`

### confint

Generate confidence intervals for R-squared and adjusted R-squared.

\
[`confint`](https://rdrr.io/r/stats/confint.html)`(``mvn``, level ``=`` ``0.95``)`\
`#>         2.5 %    97.5 %`\
`#> rsq 0.7031917 0.9058608`\
`#> adj 0.6819911 0.8991366`\
[`confint`](https://rdrr.io/r/stats/confint.html)`(``adf``, level ``=`` ``0.95``)`\
`#>         2.5 %    97.5 %`\
`#> rsq 0.7027351 0.9063174`\
`#> adj 0.6815019 0.8996258`\
[`confint`](https://rdrr.io/r/stats/confint.html)`(``hc3``, level ``=`` ``0.95``)`\
`#>         2.5 %    97.5 %`\
`#> rsq 0.6921245 0.9169280`\
`#> adj 0.6701334 0.9109943`

## References

Dudgeon, P. (2017). Some improvements in confidence intervals for
standardized regression coefficients. *Psychometrika*, *82*(4), 928–951.
<https://doi.org/10.1007/s11336-017-9563-z>

National Research Council. (1982). *An assessment of research-doctorate
programs in the United States: Social and behavioral sciences*. National
Academies Press. <https://doi.org/10.17226/9781>

Pesigan, I. J. A., Sun, R. W., & Cheung, S. F. (2023). betaDelta and
betaSandwich: Confidence intervals for standardized regression
coefficients in R. *Multivariate Behavioral Research*, *58*(6),
1183–1186. <https://doi.org/10.1080/00273171.2023.2201277>
