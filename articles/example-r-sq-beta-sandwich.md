# betaSandwich: Example Using the RSqBetaSandwich Function

Confidence intervals for multiple correlation are generated using the
[`RSqBetaSandwich()`](https://github.com/jeksterslab/betaSandwich/reference/RSqBetaSandwich.md)
function from the `betaSandwich` package. In this example, we use the
data set and the model used in [betaSandwich: Example Using the BetaHC
Function](https://github.com/jeksterslab/betaSandwich/articles/example-beta-sandwich.md).

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`betaSandwich`](https://github.com/jeksterslab/betaSandwich)`)`

\
`df`` ``<-`` ``betaSandwich``::`[`nas1982`](https://github.com/jeksterslab/betaSandwich/reference/nas1982.md)

## Fit the regression model using the `lm()` function.

\
`object`` ``<-`` `[`lm`](https://rdrr.io/r/stats/lm.html)`(``QUALITY`` ``~`` ``NARTIC`` ``+`` ``PCTGRT`` ``+`` ``PCTSUPP``, data ``=`` ``df``)`

## Estimate the standardized regression slopes and the corresponding sampling covariance matrix.

#### Multivariate Normal-Theory Approach

\
`std_mvn`` ``<-`` `[`BetaN`](https://github.com/jeksterslab/betaSandwich/reference/BetaN.md)`(``object``)`

#### Asymptotic Distribution-Free Approach

\
`std_adf`` ``<-`` `[`BetaADF`](https://github.com/jeksterslab/betaSandwich/reference/BetaADF.md)`(``object``)`

#### HC3

\
`std_hc3`` ``<-`` `[`BetaHC`](https://github.com/jeksterslab/betaSandwich/reference/BetaHC.md)`(``object``, type ``=`` ``"hc3"``)`

## Estimate the multiple correlation coefficients (R-squared and adjusted R-squared) and the corresponding sampling covariance matrix.

\
`mvn`` ``<-`` `[`RSqBetaSandwich`](https://github.com/jeksterslab/betaSandwich/reference/RSqBetaSandwich.md)`(``std_mvn``, alpha ``=`` ``0.05``)`\
`adf`` ``<-`` `[`RSqBetaSandwich`](https://github.com/jeksterslab/betaSandwich/reference/RSqBetaSandwich.md)`(``std_adf``, alpha ``=`` ``0.05``)`\
`hc3`` ``<-`` `[`RSqBetaSandwich`](https://github.com/jeksterslab/betaSandwich/reference/RSqBetaSandwich.md)`(``std_hc3``, alpha ``=`` ``0.05``)`

## Methods

### summary

Summary of the results of
[`RSqBetaSandwich()`](https://github.com/jeksterslab/betaSandwich/reference/RSqBetaSandwich.md).

\
[`summary`](https://rdrr.io/r/base/summary.html)`(``mvn``)`\
`#> Call:`\
`#> RSqBetaSandwich(object = std_mvn, alpha = 0.05)`\
`#> `\
`#> Multiple correlation with MVN standard errors:`\
`#>        est     se       t df p   2.5%  97.5%`\
`#> rsq 0.8045 0.0517 15.5608 42 0 0.7002 0.9089`\
`#> adj 0.7906 0.0554 14.2713 42 0 0.6788 0.9024`\
[`summary`](https://rdrr.io/r/base/summary.html)`(``adf``)`\
`#> Call:`\
`#> RSqBetaSandwich(object = std_adf, alpha = 0.05)`\
`#> `\
`#> Multiple correlation with MVN standard errors:`\
`#>        est     se       t df p   2.5%  97.5%`\
`#> rsq 0.8045 0.0519 15.4910 42 0 0.6997 0.9093`\
`#> adj 0.7906 0.0556 14.2073 42 0 0.6783 0.9029`\
[`summary`](https://rdrr.io/r/base/summary.html)`(``hc3``)`\
`#> Call:`\
`#> RSqBetaSandwich(object = std_hc3, alpha = 0.05)`\
`#> `\
`#> Multiple correlation with HC3 standard errors:`\
`#>        est     se       t df p   2.5%  97.5%`\
`#> rsq 0.8045 0.0573 14.0286 42 0 0.6888 0.9203`\
`#> adj 0.7906 0.0614 12.8662 42 0 0.6666 0.9146`

### coef

Calculate R-squared and adjusted R-squared.

\
[`coef`](https://rdrr.io/r/stats/coef.html)`(``mvn``)`\
`#>   rsq.rsq   adj.adj `\
`#> 0.8045263 0.7905638`\
[`coef`](https://rdrr.io/r/stats/coef.html)`(``adf``)`\
`#>   rsq.rsq   adj.adj `\
`#> 0.8045263 0.7905638`\
[`coef`](https://rdrr.io/r/stats/coef.html)`(``hc3``)`\
`#>   rsq.rsq   adj.adj `\
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
`#> rsq 0.7001868 0.9088657`\
`#> adj 0.6787716 0.9023561`\
[`confint`](https://rdrr.io/r/stats/confint.html)`(``adf``, level ``=`` ``0.95``)`\
`#>         2.5 %    97.5 %`\
`#> rsq 0.6997168 0.9093357`\
`#> adj 0.6782680 0.9028597`\
[`confint`](https://rdrr.io/r/stats/confint.html)`(``hc3``, level ``=`` ``0.95``)`\
`#>         2.5 %    97.5 %`\
`#> rsq 0.6887915 0.9202610`\
`#> adj 0.6665624 0.9145653`

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
