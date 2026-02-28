# betaSandwich: Example Using the DiffBetaSandwich Function

Confidence intervals for differences of standardized regression slopes
are generated using the
[`DiffBetaSandwich()`](https://github.com/jeksterslab/betaSandwich/reference/DiffBetaSandwich.md)
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

## Estimate differences of standardized regression slopes and the corresponding sampling covariance matrix.

``` r

mvn <- DiffBetaSandwich(std_mvn, alpha = 0.05)
adf <- DiffBetaSandwich(std_adf, alpha = 0.05)
hc3 <- DiffBetaSandwich(std_hc3, alpha = 0.05)
```

## Methods

### summary

Summary of the results of
[`DiffBetaSandwich()`](https://github.com/jeksterslab/betaSandwich/reference/DiffBetaSandwich.md).

``` r

summary(mvn)
#> Call:
#> DiffBetaSandwich(object = std_mvn, alpha = 0.05)
#> 
#> Difference between standardized regression coefficients with MVN standard errors:
#>                   est     se      z      p    2.5%  97.5%
#> NARTIC-PCTGRT  0.1037 0.1357 0.7640 0.4449 -0.1623 0.3696
#> NARTIC-PCTSUPP 0.2319 0.1252 1.8524 0.0640 -0.0135 0.4773
#> PCTGRT-PCTSUPP 0.1282 0.1227 1.0451 0.2960 -0.1123 0.3688
summary(adf)
#> Call:
#> DiffBetaSandwich(object = std_adf, alpha = 0.05)
#> 
#> Difference between standardized regression coefficients with MVN standard errors:
#>                   est     se      z      p    2.5%  97.5%
#> NARTIC-PCTGRT  0.1037 0.1212 0.8555 0.3923 -0.1338 0.3411
#> NARTIC-PCTSUPP 0.2319 0.1181 1.9642 0.0495  0.0005 0.4633
#> PCTGRT-PCTSUPP 0.1282 0.1215 1.0555 0.2912 -0.1099 0.3664
summary(hc3)
#> Call:
#> DiffBetaSandwich(object = std_hc3, alpha = 0.05)
#> 
#> Difference between standardized regression coefficients with HC3 standard errors:
#>                   est     se      z      p    2.5%  97.5%
#> NARTIC-PCTGRT  0.1037 0.1417 0.7316 0.4644 -0.1741 0.3814
#> NARTIC-PCTSUPP 0.2319 0.1318 1.7595 0.0785 -0.0264 0.4902
#> PCTGRT-PCTSUPP 0.1282 0.1375 0.9329 0.3509 -0.1412 0.3977
```

### coef

Calculate differences of standardized regression slopes.

``` r

coef(mvn)
#>  NARTIC-PCTGRT NARTIC-PCTSUPP PCTGRT-PCTSUPP 
#>      0.1036564      0.2318974      0.1282410
coef(adf)
#>  NARTIC-PCTGRT NARTIC-PCTSUPP PCTGRT-PCTSUPP 
#>      0.1036564      0.2318974      0.1282410
coef(hc3)
#>  NARTIC-PCTGRT NARTIC-PCTSUPP PCTGRT-PCTSUPP 
#>      0.1036564      0.2318974      0.1282410
```

### vcov

Calculate the sampling covariance matrix of differences of standardized
regression slopes.

``` r

vcov(mvn)
#>                NARTIC-PCTGRT NARTIC-PCTSUPP PCTGRT-PCTSUPP
#> NARTIC-PCTGRT    0.018408653    0.009511262   -0.008897391
#> NARTIC-PCTSUPP   0.009511262    0.015672035    0.006160773
#> PCTGRT-PCTSUPP  -0.008897391    0.006160773    0.015058164
vcov(adf)
#>                NARTIC-PCTGRT NARTIC-PCTSUPP PCTGRT-PCTSUPP
#> NARTIC-PCTGRT    0.014681407    0.006928651   -0.007752755
#> NARTIC-PCTSUPP   0.006928651    0.013938955    0.007010303
#> PCTGRT-PCTSUPP  -0.007752755    0.007010303    0.014763058
vcov(hc3)
#>                NARTIC-PCTGRT NARTIC-PCTSUPP PCTGRT-PCTSUPP
#> NARTIC-PCTGRT    0.020076382    0.009274583   -0.010801799
#> NARTIC-PCTSUPP   0.009274583    0.017370731    0.008096148
#> PCTGRT-PCTSUPP  -0.010801799    0.008096148    0.018897947
```

### confint

Generate confidence intervals for differences of standardized regression
slopes.

``` r

confint(mvn, level = 0.95)
#>                      2.5 %    97.5 %
#> NARTIC-PCTGRT  -0.16226855 0.3695814
#> NARTIC-PCTSUPP -0.01346652 0.4772614
#> PCTGRT-PCTSUPP -0.11226950 0.3687516
confint(adf, level = 0.95)
#>                        2.5 %    97.5 %
#> NARTIC-PCTGRT  -0.1338262589 0.3411391
#> NARTIC-PCTSUPP  0.0004975295 0.4632974
#> PCTGRT-PCTSUPP -0.1099011119 0.3663832
confint(hc3, level = 0.95)
#>                      2.5 %    97.5 %
#> NARTIC-PCTGRT  -0.17405314 0.3813660
#> NARTIC-PCTSUPP -0.02642203 0.4902169
#> PCTGRT-PCTSUPP -0.14119483 0.3976769
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
