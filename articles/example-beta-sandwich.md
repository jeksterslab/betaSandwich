# betaSandwich: Example Using the BetaHC Function

In this example, a multiple regression model is fitted using program
quality ratings (`QUALITY`) as the regressand/outcome variable and
number of published articles attributed to the program faculty members
(`NARTIC`), percent of faculty members holding research grants
(`PCTGRT`), and percentage of program graduates who received support
(`PCTSUPP`) as regressor/predictor variables using a data set from 1982
ratings of 46 doctoral programs in psychology in the USA (National
Research Council, 1982). Robust confidence intervals for the
standardized regression coefficients are generated using the
[`BetaHC()`](https://github.com/jeksterslab/betaSandwich/reference/BetaHC.md)
function from the `betaSandwich` package following Dudgeon (2017).[^1]

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

## Estimate the standardized regression slopes and the corresponding robust sampling covariance matrix.

``` r

BetaHC(object, type = "hc3", alpha = 0.05)
#> Call:
#> BetaHC(object = object, type = "hc3", alpha = 0.05)
#> 
#> Standardized regression slopes with HC3 standard errors:
#>            est     se      t df      p   2.5%  97.5%
#> NARTIC  0.4951 0.0786 6.3025 42 0.0000 0.3366 0.6537
#> PCTGRT  0.3915 0.0818 4.7831 42 0.0000 0.2263 0.5567
#> PCTSUPP 0.2632 0.0855 3.0786 42 0.0037 0.0907 0.4358
```

## Methods

``` r

out <- BetaHC(object, type = "hc3", alpha = 0.05)
```

### summary

Summary of the results of
[`BetaHC()`](https://github.com/jeksterslab/betaSandwich/reference/BetaHC.md).

``` r

summary(out)
#> Call:
#> BetaHC(object = object, type = "hc3", alpha = 0.05)
#> 
#> Standardized regression slopes with HC3 standard errors:
#>            est     se      t df      p   2.5%  97.5%
#> NARTIC  0.4951 0.0786 6.3025 42 0.0000 0.3366 0.6537
#> PCTGRT  0.3915 0.0818 4.7831 42 0.0000 0.2263 0.5567
#> PCTSUPP 0.2632 0.0855 3.0786 42 0.0037 0.0907 0.4358
```

### coef

Calculate the standardized regression slopes.

``` r

coef(out)
#>    NARTIC    PCTGRT   PCTSUPP 
#> 0.4951451 0.3914887 0.2632477
```

### vcov

Calculate the robust sampling covariance matrix of the standardized
regression slopes.

``` r

vcov(out)
#>               NARTIC       PCTGRT      PCTSUPP
#> NARTIC   0.006172168 -0.003602529 -0.001943469
#> PCTGRT  -0.003602529  0.006699155 -0.002443584
#> PCTSUPP -0.001943469 -0.002443584  0.007311625
```

### confint

Generate robust confidence intervals for standardized regression slopes.

``` r

confint(out, level = 0.95)
#>              2.5 %    97.5 %
#> NARTIC  0.33659828 0.6536920
#> PCTGRT  0.22631203 0.5566654
#> PCTSUPP 0.09068548 0.4358099
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

[^1]: The
    [`BetaN()`](https://github.com/jeksterslab/betaSandwich/reference/BetaN.md),
    and
    [`BetaADF()`](https://github.com/jeksterslab/betaSandwich/reference/BetaADF.md)
    functions are also available using the normal-theory and the
    asymptotic distribution-free approaches, respectively.
