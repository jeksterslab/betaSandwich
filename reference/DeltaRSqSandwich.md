# Estimate Improvement in R-Squared and the Corresponding Sampling Covariance Matrix

Estimate Improvement in R-Squared and the Corresponding Sampling
Covariance Matrix

## Usage

``` r
DeltaRSqSandwich(object, alpha = c(0.05, 0.01, 0.001))
```

## Arguments

- object:

  Object of class `betasandwich`, that is, the output of the
  [`BetaHC()`](https://jeksterslab.github.io/betaSandwich/reference/BetaHC.md),
  [`BetaN()`](https://jeksterslab.github.io/betaSandwich/reference/BetaN.md),
  or
  [`BetaADF()`](https://jeksterslab.github.io/betaSandwich/reference/BetaADF.md)
  functions.

- alpha:

  Numeric vector. Significance level \\(\alpha\\).

## Value

Returns an object of class `essandwich` which is a list with the
following elements:

- call:

  Function call.

- fit:

  The argument `object`.

- args:

  Function arguments.

- vcov:

  Sampling covariance matrix of improvement in R-squared (squared
  semipartial correlation coefficients).

- est:

  Vector of improvement in R-squared (squared semipartial correlation
  coefficients).

- fun:

  Function used ("DeltaRSqSandwich").

## Details

The sampling covariance matrix is obtained by the delta method from the
sampling covariance matrix of the standardized parameters stored in
`object` (normal-theory, asymptotically distribution-free, or
heteroskedasticity-consistent). Two or more regressors are required.

Let \\(\boldsymbol{\beta}^{\ast}\\) be the vector of standardized
slopes, \\(\boldsymbol{R}\_{x}\\) the correlation matrix of the
regressors, \\(r^{jj}\\) the \\(j\\)th diagonal element of
\\(\boldsymbol{R}\_{x}^{-1}\\), and \\(R^{2} =
\boldsymbol{\beta}^{\ast\prime} \boldsymbol{R}\_{x}
\boldsymbol{\beta}^{\ast}\\). The semipartial correlation of regressor
\\(j\\) is \$\$sr\_{j} = \beta^{\ast}\_{j} / \sqrt{r^{jj}} =
\beta^{\ast}\_{j} \sqrt{1 - R^{2}\_{j \cdot -j}},\$\$ where \\(R^{2}\_{j
\cdot -j}\\) is the R-squared from regressing regressor \\(j\\) on the
other regressors. Its square, \\(sr^{2}\_{j}\\), is the change in
\\(R^{2}\\) when regressor \\(j\\) is added last. Because \\(1 / r^{jj}
= 1 - R^{2}\_{j \cdot -j}\\), \\(sr\_{j}\\) shrinks toward zero as
regressor \\(j\\) becomes more redundant with the other regressors. The
improvement in R-squared for regressor \\(j\\) is \$\$\Delta R^{2}\_{j}
= R^{2} - R^{2}\_{-j} = sr^{2}\_{j}.\$\$ For an effect size \\(g\left(
\boldsymbol{\theta}^{\ast} \right)\\) that is a function of the
standardized parameters, the sampling variance is approximated by the
delta method, \$\$\widehat{\operatorname{var}} \left( \hat{g} \right) =
\boldsymbol{J}\_{g} \\,
\widehat{\boldsymbol{\Sigma}}\_{\boldsymbol{\theta}^{\ast}} \\,
\boldsymbol{J}\_{g}^{\prime},\$\$ where \\(\boldsymbol{J}\_{g}\\) is the
Jacobian of \\(g\\) and
\\(\widehat{\boldsymbol{\Sigma}}\_{\boldsymbol{\theta}^{\ast}}\\) is the
sampling covariance matrix from
[`BetaHC()`](https://jeksterslab.github.io/betaSandwich/reference/BetaHC.md),
[`BetaADF()`](https://jeksterslab.github.io/betaSandwich/reference/BetaADF.md),
or
[`BetaN()`](https://jeksterslab.github.io/betaSandwich/reference/BetaN.md).

## References

Algina, J., Keselman, H. J., & Penfield, R. D. (2007). Confidence
intervals for an effect size measure in multiple linear regression.
*Educational and Psychological Measurement*, *67*(2), 207–218.
[doi:10.1177/0013164406292030](https://doi.org/10.1177/0013164406292030)

Algina, J., Keselman, H. J., & Penfield, R. D. (2008a). Confidence
intervals for the squared multiple semipartial correlation coefficient.
*Journal of Modern Applied Statistical Methods*, *7*(1), 2–10.
[doi:10.22237/jmasm/1209614460](https://doi.org/10.22237/jmasm/1209614460)

Algina, J., Keselman, H. J., & Penfield, R. D. (2008b). Note on a
confidence interval for the squared semipartial correlation coefficient.
*Educational and Psychological Measurement*, *68*(5), 734–741.
[doi:10.1177/0013164407313371](https://doi.org/10.1177/0013164407313371)

Algina, J., Keselman, H. J., & Penfield, R. D. (2010). Confidence
intervals for squared semipartial correlation coefficients: The effect
of nonnormality. *Educational and Psychological Measurement*, *70*(6),
926–940.
[doi:10.1177/0013164410379335](https://doi.org/10.1177/0013164410379335)

## See also

Other Beta Sandwich Functions:
[`BetaADF()`](https://jeksterslab.github.io/betaSandwich/reference/BetaADF.md),
[`BetaHC()`](https://jeksterslab.github.io/betaSandwich/reference/BetaHC.md),
[`BetaN()`](https://jeksterslab.github.io/betaSandwich/reference/BetaN.md),
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
object <- stats::lm(
  formula = QUALITY ~ NARTIC + PCTGRT + PCTSUPP,
  data = nas1982
)
std <- BetaHC(object = object)
out <- DeltaRSqSandwich(object = std)
# Methods -------------------------------------------------------
print(x = out)
#> Call:
#> DeltaRSqSandwich(object = std)
#> 
#> Improvement in R-squared with HC3 standard errors:
#>            est     se      z      p   0.05%    0.5%    2.5%  97.5%  99.5%
#> NARTIC  0.1859 0.0685 2.7151 0.0066 -0.0394  0.0095  0.0517 0.3201 0.3622
#> PCTGRT  0.1177 0.0541 2.1734 0.0298 -0.0605 -0.0218  0.0116 0.2238 0.2571
#> PCTSUPP 0.0569 0.0373 1.5233 0.1277 -0.0660 -0.0393 -0.0163 0.1300 0.1530
#>         99.95%
#> NARTIC  0.4112
#> PCTGRT  0.2958
#> PCTSUPP 0.1797
summary(object = out)
#> Call:
#> DeltaRSqSandwich(object = std)
#> 
#> Improvement in R-squared with HC3 standard errors:
#>            est     se      z      p   0.05%    0.5%    2.5%  97.5%  99.5%
#> NARTIC  0.1859 0.0685 2.7151 0.0066 -0.0394  0.0095  0.0517 0.3201 0.3622
#> PCTGRT  0.1177 0.0541 2.1734 0.0298 -0.0605 -0.0218  0.0116 0.2238 0.2571
#> PCTSUPP 0.0569 0.0373 1.5233 0.1277 -0.0660 -0.0393 -0.0163 0.1300 0.1530
#>         99.95%
#> NARTIC  0.4112
#> PCTGRT  0.2958
#> PCTSUPP 0.1797
coef(object = out)
#>    NARTIC    PCTGRT   PCTSUPP 
#> 0.1858925 0.1176542 0.0568722 
vcov(object = out)
#>                NARTIC        PCTGRT       PCTSUPP
#> NARTIC   0.0046874969 -0.0001498081 -0.0001799337
#> PCTGRT  -0.0001498081  0.0029304696 -0.0003317602
#> PCTSUPP -0.0001799337 -0.0003317602  0.0013939512
confint(object = out, level = 0.95)
#>               2.5 %    97.5 %
#> NARTIC   0.05170296 0.3200820
#> PCTGRT   0.01155384 0.2237545
#> PCTSUPP -0.01630434 0.1300487
```
