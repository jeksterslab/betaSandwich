# betaSandwich: Robust Confidence Intervals for Standardized Regression Coefficients

Generates robust confidence intervals for standardized regression
coefficients using heteroskedasticity-consistent standard errors for
models fitted by [`stats::lm()`](https://rdrr.io/r/stats/lm.html)
(Dudgeon, 2017). Sampling covariance matrices can also be obtained under
multivariate normal theory and the asymptotically distribution-free
approach.

## Standardized regression coefficients

[`BetaHC()`](https://jeksterslab.github.io/betaSandwich/reference/BetaHC.md),
[`BetaADF()`](https://jeksterslab.github.io/betaSandwich/reference/BetaADF.md),
and
[`BetaN()`](https://jeksterslab.github.io/betaSandwich/reference/BetaN.md)
estimate the standardized regression coefficients and the corresponding
sampling covariance matrix.
[`BetaHC()`](https://jeksterslab.github.io/betaSandwich/reference/BetaHC.md)
is recommended in most situations.

## Functions of the standardized coefficients

[`RSqSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/RSqSandwich.md)
(R-squared and adjusted R-squared),
[`DiffBetaSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/DiffBetaSandwich.md)
(differences of standardized slopes),
[`SCorSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/SCorSandwich.md)
and
[`SCorSqSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/SCorSqSandwich.md)
(semipartial correlations),
[`PCorSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/PCorSandwich.md)
(squared partial correlations),
[`DeltaRSqSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/DeltaRSqSandwich.md)
(improvement in R-squared), and
[`FSqSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/FSqSandwich.md)
(Cohen's f-squared and local f-squared).

## Methods

The [`coef()`](https://rdrr.io/r/stats/coef.html),
[`vcov()`](https://rdrr.io/r/stats/vcov.html),
[`confint()`](https://rdrr.io/r/stats/confint.html),
[`summary()`](https://rdrr.io/r/base/summary.html), and
[`print()`](https://rdrr.io/r/base/print.html) methods are available for
objects of class `betasandwich` and `essandwich`.

## References

Dudgeon, P. (2017). Some improvements in confidence intervals for
standardized regression coefficients. *Psychometrika*, *82*(4), 928–951.
[doi:10.1007/s11336-017-9563-z](https://doi.org/10.1007/s11336-017-9563-z)

Pesigan, I. J. A., Sun, R. W., & Cheung, S. F. (2023). betaDelta and
betaSandwich: Confidence intervals for standardized regression
coefficients in R. *Multivariate Behavioral Research*.
[doi:10.1080/00273171.2023.2201277](https://doi.org/10.1080/00273171.2023.2201277)

## See also

Useful links:

- <https://github.com/jeksterslab/betaSandwich>

- <https://jeksterslab.github.io/betaSandwich/>

- Report bugs at <https://github.com/jeksterslab/betaSandwich/issues>

## Author

**Maintainer**: Ivan Jacob Agaloos Pesigan <r.jeksterslab@gmail.com>
([ORCID](https://orcid.org/0000-0003-4818-8420)) \[copyright holder\]

Authors:

- Ivan Jacob Agaloos Pesigan <r.jeksterslab@gmail.com>
  ([ORCID](https://orcid.org/0000-0003-4818-8420)) \[copyright holder\]
