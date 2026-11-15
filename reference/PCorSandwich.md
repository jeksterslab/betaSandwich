# Estimate Squared Partial Correlation Coefficients and the Corresponding Sampling Covariance Matrix

Estimate Squared Partial Correlation Coefficients and the Corresponding
Sampling Covariance Matrix

## Usage

``` r
PCorSandwich(object, alpha = c(0.05, 0.01, 0.001))
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

  Sampling covariance matrix of squared partial correlation
  coefficients.

- est:

  Vector of squared partial correlation coefficients.

- fun:

  Function used ("PCorSandwich").

## Details

The sampling covariance matrix is obtained by the delta method from the
sampling covariance matrix of the standardized parameters stored in
`object` (normal-theory, asymptotically distribution-free, or
heteroskedasticity-consistent). Two or more regressors are required.

Let \\(\boldsymbol{\beta}^{\ast}\\) be the vector of standardized
slopes, \\(\mathbf{R}\_{x}\\) the correlation matrix of the regressors,
\\(r^{jj}\\) the \\(j\\)th diagonal element of
\\(\mathbf{R}\_{x}^{-1}\\), and \\(R^{2} =
\boldsymbol{\beta}^{\ast\prime} \mathbf{R}\_{x}
\boldsymbol{\beta}^{\ast}\\). The semipartial correlation of regressor
\\(j\\) is \$\$sr\_{j} = \beta^{\ast}\_{j} / \sqrt{r^{jj}} =
\beta^{\ast}\_{j} \sqrt{1 - R^{2}\_{j \cdot -j}},\$\$ where \\(R^{2}\_{j
\cdot -j}\\) is the R-squared from regressing regressor \\(j\\) on the
other regressors. Its square, \\(sr^{2}\_{j}\\), is the change in
\\(R^{2}\\) when regressor \\(j\\) is added last. Because \\(1 / r^{jj}
= 1 - R^{2}\_{j \cdot -j}\\), \\(sr\_{j}\\) shrinks toward zero as
regressor \\(j\\) becomes more redundant with the other regressors. The
squared partial correlation is \$\$pr^{2}\_{j} = \frac{sr^{2}\_{j}}{1 -
R^{2} + sr^{2}\_{j}},\$\$ the proportion of the variance in the outcome
not explained by the other regressors that is explained by regressor
\\(j\\). For an effect size \\(g\left( \boldsymbol{\theta}^{\ast}
\right)\\) that is a function of the standardized parameters, the
sampling variance is approximated by the delta method,
\$\$\widehat{\mathrm{var}} \left( \hat{g} \right) = \mathbf{J}\_{g} \\,
\widehat{\boldsymbol{\Sigma}}\_{\boldsymbol{\theta}^{\ast}} \\,
\mathbf{J}\_{g}^{\prime},\$\$ where \\(\mathbf{J}\_{g}\\) is the
Jacobian of \\(g\\) and
\\(\widehat{\boldsymbol{\Sigma}}\_{\boldsymbol{\theta}^{\ast}}\\) is the
sampling covariance matrix from
[`BetaHC()`](https://jeksterslab.github.io/betaSandwich/reference/BetaHC.md),
[`BetaADF()`](https://jeksterslab.github.io/betaSandwich/reference/BetaADF.md),
or
[`BetaN()`](https://jeksterslab.github.io/betaSandwich/reference/BetaN.md).

## See also

Other Beta Sandwich Functions:
[`BetaADF()`](https://jeksterslab.github.io/betaSandwich/reference/BetaADF.md),
[`BetaHC()`](https://jeksterslab.github.io/betaSandwich/reference/BetaHC.md),
[`BetaN()`](https://jeksterslab.github.io/betaSandwich/reference/BetaN.md),
[`DeltaRSqSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/DeltaRSqSandwich.md),
[`DiffBetaSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/DiffBetaSandwich.md),
[`FSqSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/FSqSandwich.md),
[`RSqSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/RSqSandwich.md),
[`SCorSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/SCorSandwich.md),
[`SCorSqSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/SCorSqSandwich.md)

## Author

Ivan Jacob Agaloos Pesigan

## Examples

``` r
object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
std <- BetaHC(object)
out <- PCorSandwich(std)
# Methods -------------------------------------------------------
print(out)
#> Call:
#> PCorSandwich(object = std)
#> 
#> Squared partial correlations with HC3 standard errors:
#>            est     se      z      p   0.05%    0.5%    2.5%  97.5%  99.5%
#> NARTIC  0.4874 0.1095 4.4532 0.0000  0.1273  0.2055  0.2729 0.7020 0.7694
#> PCTGRT  0.3757 0.1156 3.2500 0.0012 -0.0047  0.0779  0.1491 0.6023 0.6735
#> PCTSUPP 0.2254 0.1277 1.7647 0.0776 -0.1949 -0.1036 -0.0249 0.4757 0.5543
#>         99.95%
#> NARTIC  0.8476
#> PCTGRT  0.7562
#> PCTSUPP 0.6456
summary(out)
#> Call:
#> PCorSandwich(object = std)
#> 
#> Squared partial correlations with HC3 standard errors:
#>            est     se      z      p   0.05%    0.5%    2.5%  97.5%  99.5%
#> NARTIC  0.4874 0.1095 4.4532 0.0000  0.1273  0.2055  0.2729 0.7020 0.7694
#> PCTGRT  0.3757 0.1156 3.2500 0.0012 -0.0047  0.0779  0.1491 0.6023 0.6735
#> PCTSUPP 0.2254 0.1277 1.7647 0.0776 -0.1949 -0.1036 -0.0249 0.4757 0.5543
#>         99.95%
#> NARTIC  0.8476
#> PCTGRT  0.7562
#> PCTSUPP 0.6456
coef(out)
#>    NARTIC    PCTGRT   PCTSUPP 
#> 0.4874382 0.3757383 0.2253739 
vcov(out)
#>              NARTIC       PCTGRT      PCTSUPP
#> NARTIC  0.011980986 0.0021633722 0.0027577200
#> PCTGRT  0.002163372 0.0133662295 0.0007428294
#> PCTSUPP 0.002757720 0.0007428294 0.0163111322
confint(out, level = 0.95)
#>               2.5 %    97.5 %
#> NARTIC   0.27290509 0.7019714
#> PCTGRT   0.14914218 0.6023345
#> PCTSUPP -0.02494295 0.4756908
```
