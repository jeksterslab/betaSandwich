#' Estimate Standardized Regression Coefficients
#' and the Corresponding Robust Sampling Covariance Matrix
#' Using the Heteroskedasticity Consistent Approach
#'
#' @author Ivan Jacob Agaloos Pesigan
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
#' @param type Character string.
#'   Correction type.
#'   Possible values are
#'   `"hc0"`,
#'   `"hc1"`,
#'   `"hc2"`,
#'   `"hc3"`,
#'   `"hc4"`,
#'   `"hc4m"`, and
#'   `"hc5"`.
#' @param alpha Numeric vector.
#'   Significance level \eqn{\alpha}.
#' @param g1 Numeric.
#'   `g1` value for `type = "hc4m"`.
#' @param g2 Numeric.
#'   `g2` value for `type = "hc4m"`.
#' @param k Numeric.
#'   Constant `k` for `type = "hc5"`
#'   \eqn{0 \leq k \leq 1}.
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
#' std <- BetaHC(object)
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
BetaHC <- function(object,
                   type = "hc3",
                   alpha = c(0.05, 0.01, 0.001),
                   g1 = 1,
                   g2 = 1.5,
                   k = 0.7) {
  stopifnot(
    inherits(
      x = object,
      what = "lm"
    )
  )
  lm_process <- .ProcessLM(object)
  stopifnot(
    type %in% c(
      "hc0",
      "hc1",
      "hc2",
      "hc3",
      "hc4",
      "hc4m",
      "hc5"
    )
  )
  stopifnot(0 < k & k < 1)
  constant <- k
  gammacap <- .GammaHC(
    d = .DofMat(
      lm_process$x,
      center = colMeans(lm_process$x),
      n = lm_process$n,
      k = lm_process$k
    ),
    sigmacap = lm_process$sigmacap,
    qcap = .QMat(
      h = stats::hatvalues(object),
      k = lm_process$k,
      type = type,
      g1 = g1,
      g2 = g2,
      constant = constant
    ),
    n = lm_process$n
  )
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
  wcap <- chol2inv(chol(gammacap_mvn))
  acov <- .ACovHC(
    jcap = jcap,
    gammacap = gammacap,
    gammacap_mvn = gammacap_mvn
  )
  vcov <- .CovHC(
    acov = acov,
    type = type,
    n = lm_process$n,
    df = lm_process$df
  )[
    seq_len(lm_process$p),
    seq_len(lm_process$p),
    drop = FALSE
  ]
  colnames(vcov) <- rownames(vcov) <- lm_process$xnames
  out <- list(
    call = match.call(),
    args = list(
      object = object,
      type = type,
      alpha = alpha,
      g1 = g1,
      g2 = g2,
      k = k
    ),
    lm_process = lm_process,
    gamma_n = gammacap_mvn,
    gamma_hc = gammacap,
    gamma = (
      gammacap_mvn %*% (
        wcap %*% gammacap %*% wcap
      ) %*% gammacap_mvn
    ),
    acov = acov,
    vcov = vcov,
    est = lm_process$betastar
  )
  class(out) <- c(
    "betasandwich",
    class(out)
  )
  out
}
