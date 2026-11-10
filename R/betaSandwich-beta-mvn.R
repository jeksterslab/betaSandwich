#' Estimate Standardized Regression Coefficients
#' and the Corresponding Sampling Covariance Matrix
#' Assuming Multivariate Normality
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @details
#' Note that while the calculation in [BetaN()]
#' is different from [betaDelta::BetaDelta()] with `type = "mvn"`,
#' the results are numerically equivalent.
#' [BetaN()] assumes multivariate normality.
#' [BetaHC()] is recommended in most situations.
#'
#' @return Returns an object
#'   of class `betasandwich` which is a list with the following elements:
#'   \describe{
#'     \item{call}{Function call.}
#'     \item{args}{Function arguments.}
#'     \item{lm_process}{Processed `lm` object.}
#'     \item{gamma_n}{Asymptotic covariance matrix
#'       of the sample covariance matrix
#'       assuming multivariate normality.}
#'     \item{gamma_hc}{Asymptotic covariance matrix
#'       HC correction.}
#'     \item{gamma}{Asymptotic covariance matrix
#'       of the sample covariance matrix.}
#'     \item{acov}{Asymptotic covariance matrix
#'       of the standardized slopes.}
#'     \item{vcov}{Sampling covariance matrix
#'       of the standardized slopes.}
#'     \item{est}{Vector of standardized slopes.}
#'   }
#'
#' @param object Object of class `lm`.
#' @param std_y Logical.
#'   Standardize \eqn{y}.
#'   With `std_y = FALSE`, \eqn{y} is left in its original units
#'   (\eqn{b_j \sigma_{x_j}}).
#' @param std_x Character vector or `NULL`.
#'   Names of the regressors to standardize.
#'   `NULL` (default) standardizes all regressors;
#'   `character(0)` standardizes none (\eqn{b_j / \sigma_y}).
#'   Leaving a dummy variable out keeps its 0-1 contrast.
#'   The delta method accounts for the sampling variability
#'   of the standard deviations.
#' @param alpha Numeric vector.
#'   Significance level \eqn{\alpha}.
#'
#' @references
#' Dudgeon, P. (2017).
#' Some improvements in confidence intervals
#' for standardized regression coefficients.
#' *Psychometrika*, *82*(4), 928--951.
#' \doi{10.1007/s11336-017-9563-z}
#'
#' Pesigan, I. J. A., Sun, R. W., & Cheung, S. F. (2023).
#' betaDelta and betaSandwich:
#' Confidence intervals for standardized regression coefficients in R.
#' *Multivariate Behavioral Research*.
#' \doi{10.1080/00273171.2023.2201277}
#'
#' @examples
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' std <- BetaN(object)
#' # Methods -------------------------------------------------------
#' print(std)
#' summary(std)
#' coef(std)
#' vcov(std)
#' confint(std, level = 0.95)
#'
#' @family Beta Sandwich Functions
#' @keywords betaSandwich std
#' @export
BetaN <- function(object,
                  alpha = c(0.05, 0.01, 0.001),
                  std_y = TRUE,
                  std_x = NULL) {
  stopifnot(
    inherits(
      x = object,
      what = "lm"
    )
  )
  lm_process <- .ProcessLM(object = object)
  index <- .BetaStarSelectIndex(
    std_x = std_x,
    xnames = lm_process$xnames
  )
  jcap <- .JacVechSigmaWRTThetaStar(
    betastar = lm_process$betastar,
    sigmay = lm_process$sigma[1],
    sigmax = lm_process$sigma[-1],
    rhocapx = lm_process$rhocap[
      2:lm_process$k,
      2:lm_process$k,
      drop = FALSE
    ],
    q = lm_process$q,
    p = lm_process$p
  )
  gammacap_mvn <- .GammaN(
    sigmacap = lm_process$sigmacap,
    pinv_of_dcap = .PInvDmat(d = .DMat(k = lm_process$k))
  )
  acov <- chol2inv(
    x = chol(
      x = .ACovSEMInverse(
        jcap = jcap,
        acov = gammacap_mvn
      )
    )
  )
  vcov <- (1 / lm_process$n) * acov
  vcov <- .BetaStarSelectVCov(
    vcov = vcov,
    lm_process = lm_process,
    std_y = std_y,
    std_x = index
  )
  colnames(x = vcov) <- rownames(x = vcov) <- lm_process$xnames
  out <- list(
    call = match.call(),
    args = list(
      object = object,
      type = "mvn",
      alpha = alpha,
      std_y = std_y,
      std_x = std_x
    ),
    lm_process = lm_process,
    gamma_n = gammacap_mvn,
    gamma_hc = NULL,
    gamma = gammacap_mvn,
    acov = acov,
    vcov = vcov,
    est = .BetaStarSelectLM(
      lm_process = lm_process,
      std_y = std_y,
      std_x = index
    )
  )
  attributes(x = out)$std <- std_y && all(index)
  class(x = out) <- c(
    "betasandwich",
    class(x = out)
  )
  out
}
