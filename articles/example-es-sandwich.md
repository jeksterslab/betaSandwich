# betaSandwich: Example Using the SCorSandwich, PCorSandwich, DeltaRSqSandwich, and FSqSandwich Functions

Confidence intervals for semipartial correlations, squared partial
correlations, the improvement in R-squared, and Cohen’s f-squared are
generated using the
[`SCorSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/SCorSandwich.md),
[`PCorSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/PCorSandwich.md),
[`DeltaRSqSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/DeltaRSqSandwich.md),
and
[`FSqSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/FSqSandwich.md)
functions from the `betaSandwich` package. In this example, we use the
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

## Semipartial correlations

\
[`summary`](https://rdrr.io/r/base/summary.html)`(`[`SCorSandwich`](https://jeksterslab.github.io/betaSandwich/reference/SCorSandwich.md)`(``std_hc3``, alpha ``=`` ``0.05``)``)`\
`#> Call:`\
`#> SCorSandwich(object = std_hc3, alpha = 0.05)`\
`#> `\
`#> Semipartial correlations with HC3 standard errors:`\
`#>            est     se      z      p   2.5%  97.5%`\
`#> NARTIC  0.4312 0.0794 5.4303 0.0000 0.2755 0.5868`\
`#> PCTGRT  0.3430 0.0789 4.3468 0.0000 0.1883 0.4977`\
`#> PCTSUPP 0.2385 0.0783 3.0465 0.0023 0.0851 0.3919`

## Squared partial correlations

\
[`summary`](https://rdrr.io/r/base/summary.html)`(`[`PCorSandwich`](https://jeksterslab.github.io/betaSandwich/reference/PCorSandwich.md)`(``std_hc3``, alpha ``=`` ``0.05``)``)`\
`#> Call:`\
`#> PCorSandwich(object = std_hc3, alpha = 0.05)`\
`#> `\
`#> Squared partial correlations with HC3 standard errors:`\
`#>            est     se      z      p    2.5%  97.5%`\
`#> NARTIC  0.4874 0.1095 4.4532 0.0000  0.2729 0.7020`\
`#> PCTGRT  0.3757 0.1156 3.2500 0.0012  0.1491 0.6023`\
`#> PCTSUPP 0.2254 0.1277 1.7647 0.0776 -0.0249 0.4757`

## Improvement in R-squared

\
[`summary`](https://rdrr.io/r/base/summary.html)`(`[`DeltaRSqSandwich`](https://jeksterslab.github.io/betaSandwich/reference/DeltaRSqSandwich.md)`(``std_hc3``, alpha ``=`` ``0.05``)``)`\
`#> Call:`\
`#> DeltaRSqSandwich(object = std_hc3, alpha = 0.05)`\
`#> `\
`#> Improvement in R-squared with HC3 standard errors:`\
`#>            est     se      z      p    2.5%  97.5%`\
`#> NARTIC  0.1859 0.0685 2.7151 0.0066  0.0517 0.3201`\
`#> PCTGRT  0.1177 0.0541 2.1734 0.0298  0.0116 0.2238`\
`#> PCTSUPP 0.0569 0.0373 1.5233 0.1277 -0.0163 0.1300`

## Cohen’s f-squared and local f-squared

\
[`summary`](https://rdrr.io/r/base/summary.html)`(`[`FSqSandwich`](https://jeksterslab.github.io/betaSandwich/reference/FSqSandwich.md)`(``std_hc3``, alpha ``=`` ``0.05``)``)`\
`#> Call:`\
`#> FSqSandwich(object = std_hc3, alpha = 0.05)`\
`#> `\
`#> Cohen's f-squared (fsq) and local f-squared with HC3 standard errors:`\
`#>            est     se      z      p    2.5%  97.5%`\
`#> fsq     4.1158 1.5009 2.7422 0.0061  1.1741 7.0575`\
`#> NARTIC  0.9510 0.4166 2.2825 0.0225  0.1344 1.7676`\
`#> PCTGRT  0.6019 0.2967 2.0288 0.0425  0.0204 1.1834`\
`#> PCTSUPP 0.2909 0.2128 1.3670 0.1716 -0.1262 0.7081`

## Comparing covariance estimators

\
[`confint`](https://rdrr.io/r/stats/confint.html)`(`[`DeltaRSqSandwich`](https://jeksterslab.github.io/betaSandwich/reference/DeltaRSqSandwich.md)`(``std_mvn``)``, level ``=`` ``0.95``)`\
`#>                2.5 %    97.5 %`\
`#> NARTIC   0.055421734 0.3163632`\
`#> PCTGRT   0.017881671 0.2174267`\
`#> PCTSUPP -0.008779331 0.1225237`\
[`confint`](https://rdrr.io/r/stats/confint.html)`(`[`DeltaRSqSandwich`](https://jeksterslab.github.io/betaSandwich/reference/DeltaRSqSandwich.md)`(``std_adf``)``, level ``=`` ``0.95``)`\
`#>                2.5 %    97.5 %`\
`#> NARTIC   0.072394224 0.2993907`\
`#> PCTGRT   0.025067308 0.2102410`\
`#> PCTSUPP -0.008691312 0.1224357`\
[`confint`](https://rdrr.io/r/stats/confint.html)`(`[`DeltaRSqSandwich`](https://jeksterslab.github.io/betaSandwich/reference/DeltaRSqSandwich.md)`(``std_hc3``)``, level ``=`` ``0.95``)`\
`#>               2.5 %    97.5 %`\
`#> NARTIC   0.05170296 0.3200820`\
`#> PCTGRT   0.01155384 0.2237545`\
`#> PCTSUPP -0.01630434 0.1300487`

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
