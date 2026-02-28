# betaSandwich: Example Using the RSqBetaSandwich Function

Confidence intervals for multiple correlation are generated using the
[`RSqBetaSandwich()`](https://github.com/jeksterslab/betaSandwich/reference/RSqBetaSandwich.md)
function from the `betaSandwich` package. In this example, we use the
data set and the model used in [betaSandwich: Example Using the BetaHC
Function](https://github.com/jeksterslab/betaSandwich/articles/example-beta-sandwich.md).

``` r

library(betaSandwich)
```

``` r

df <- betaSandwich::nas1982
```

## Fit the regression model using the `lm()` function.

``` r

object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = df)
```

## Estimate the standardized regression slopes and the corresponding sampling covariance matrix.

#### Multivariate Normal-Theory Approach

``` r

std_mvn <- BetaN(object)
```

#### Asymptotic Distribution-Free Approach

``` r

std_adf <- BetaADF(object)
```

#### HC3

``` r

std_hc3 <- BetaHC(object, type = "hc3")
```

## Estimate the multiple correlation coefficients (R-squared and adjusted R-squared) and the corresponding sampling covariance matrix.

``` r

mvn <- RSqBetaSandwich(std_mvn, alpha = 0.05)
adf <- RSqBetaSandwich(std_adf, alpha = 0.05)
hc3 <- RSqBetaSandwich(std_hc3, alpha = 0.05)
```

## Methods

### summary

Summary of the results of
[`RSqBetaSandwich()`](https://github.com/jeksterslab/betaSandwich/reference/RSqBetaSandwich.md).

``` r

summary(mvn)
#> Call:
#> RSqBetaSandwich(object = std_mvn, alpha = 0.05)
#> 
#> Multiple correlation with MVN standard errors:
#>        est     se       t df p   2.5%  97.5%
#> rsq 0.8045 0.0328 24.5344 42 0 0.7383 0.8707
#> adj 0.7906 0.0351 22.5014 42 0 0.7197 0.8615
summary(adf)
#> Call:
#> RSqBetaSandwich(object = std_adf, alpha = 0.05)
#> 
#> Multiple correlation with MVN standard errors:
#>        est     se       t df p   2.5%  97.5%
#> rsq 0.8045 0.0287 28.0431 42 0 0.7466 0.8624
#> adj 0.7906 0.0307 25.7193 42 0 0.7285 0.8526
summary(hc3)
#> Call:
#> RSqBetaSandwich(object = std_hc3, alpha = 0.05)
#> 
#> Multiple correlation with HC3 standard errors:
#>        est     se       t df p   2.5%  97.5%
#> rsq 0.8045 0.0313 25.6916 42 0 0.7413 0.8677
#> adj 0.7906 0.0336 23.5627 42 0 0.7229 0.8583
```

### coef

Calculate R-squared and adjusted R-squared.

``` r

coef(mvn)
#>   rsq.rsq   adj.adj 
#> 0.8045263 0.7905638
coef(adf)
#>   rsq.rsq   adj.adj 
#> 0.8045263 0.7905638
coef(hc3)
#>   rsq.rsq   adj.adj 
#> 0.8045263 0.7905638
```

### vcov

Calculate the sampling covariance matrix of R-squared and adjusted
R-squared.

``` r

vcov(mvn)
#>             rsq         adj
#> rsq 0.001075300 0.001152107
#> adj 0.001152107 0.001234400
vcov(adf)
#>              rsq          adj
#> rsq 0.0008230557 0.0008818454
#> adj 0.0008818454 0.0009448343
vcov(hc3)
#>              rsq         adj
#> rsq 0.0009806163 0.001050660
#> adj 0.0010506603 0.001125707
```

### confint

Generate confidence intervals for R-squared and adjusted R-squared.

``` r

confint(mvn, level = 0.95)
#>         2.5 %    97.5 %
#> rsq 0.7383498 0.8707027
#> adj 0.7196605 0.8614672
confint(adf, level = 0.95)
#>         2.5 %    97.5 %
#> rsq 0.7466296 0.8624229
#> adj 0.7285317 0.8525960
confint(hc3, level = 0.95)
#>         2.5 %    97.5 %
#> rsq 0.7413304 0.8677221
#> adj 0.7228540 0.8582736
```

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
