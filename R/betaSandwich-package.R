#' betaSandwich: Robust Confidence Intervals for Standardized Regression
#' Coefficients
#'
#' Generates robust confidence intervals for standardized regression
#' coefficients using heteroskedasticity-consistent standard errors for models
#' fitted by [stats::lm()] (Dudgeon, 2017). Sampling covariance matrices can
#' also be obtained under multivariate normal theory and the asymptotically
#' distribution-free approach.
#'
#' @section Standardized regression coefficients:
#' [BetaHC()], [BetaADF()], and [BetaN()] estimate the standardized regression
#' coefficients and the corresponding sampling covariance matrix.
#' [BetaHC()] is recommended in most situations.
#'
#' @section Functions of the standardized coefficients:
#' [RSqSandwich()] (R-squared and adjusted R-squared),
#' [DiffBetaSandwich()] (differences of standardized slopes),
#' [SCorSandwich()] and [SCorSqSandwich()] (semipartial correlations),
#' [PCorSandwich()] (squared partial correlations),
#' [DeltaRSqSandwich()] (improvement in R-squared), and
#' [FSqSandwich()] (Cohen's f-squared and local f-squared).
#'
#' @section Methods:
#' The `coef()`, `vcov()`, `confint()`, `summary()`, and `print()` methods are
#' available for objects of class `betasandwich` and `essandwich`.
#'
#' @references
#' Dudgeon, P. (2017).
#' Some improvements in confidence intervals for standardized regression
#' coefficients.
#' *Psychometrika*, *82*(4), 928--951.
#' \doi{10.1007/s11336-017-9563-z}
#'
#' Pesigan, I. J. A., Sun, R. W., & Cheung, S. F. (2023).
#' betaDelta and betaSandwich:
#' Confidence intervals for standardized regression coefficients in R.
#' *Multivariate Behavioral Research*.
#' \doi{10.1080/00273171.2023.2201277}
#'
#' @aliases betaSandwich-package
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
