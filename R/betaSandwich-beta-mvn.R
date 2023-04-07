#' Estimate Standardized Regression Coefficients
#' and the Corresponding Sampling Covariance Matrix
#' Assuming Multivariate Normality
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @details
#' Note that while the calculation in `BetaN()`
#' is different from `betaDelta::BetaDelta()` with `type = "mvn"`,
#' the results are numerically equivalent.
#' `BetaN()` assumes multivariate normality.
#' `BetaHC()` is recommended in most situations.
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
#'
#' @references
#' Dudgeon, P. (2017).
#' Some improvements in confidence intervals
#' for standardized regression coefficients.
#' *Psychometrika*, *82*(4), 928–951.
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
#' @export
#' @family Beta Sandwich Functions
#' @keywords betaSandwich std
BetaN <- function(object) {
  lm_process <- .ProcessLM(object)
  jcap <- .JacobianVechSigmaWRTThetaStar(
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
    pinv_of_dcap = .PInvDmat(.DMat(lm_process$k))
  )
  acov <- chol2inv(
    chol(
      .ACovSEMInverse(
        jcap = jcap,
        acov = gammacap_mvn
      )
    )
  )
  vcov <- (1 / lm_process$n) * acov
  vcov <- vcov[
    seq_len(lm_process$p),
    seq_len(lm_process$p),
    drop = FALSE
  ]
  colnames(vcov) <- rownames(vcov) <- lm_process$xnames
  out <- list(
    call = match.call(),
    args = list(
      object = object,
      type = "mvn"
    ),
    lm_process = lm_process,
    gamma_n = gammacap_mvn,
    gamma_hc = NULL,
    gamma = gammacap_mvn,
    acov = acov,
    vcov = vcov,
    est = lm_process$betastar
  )
  class(out) <- c(
    "betasandwich",
    class(out)
  )
  return(
    out
  )
}
