#' Estimate Standardized Regression Coefficients
#' and Robust Sampling Covariance Matrix
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns an object of class `betasandwich`
#' which is a list with the following elements:
#' \describe{
#'   \item{call}{Function call.}
#'   \item{lm}{Object of class `lm`.}
#'   \item{lm_process}{Pre-processed object of class `lm`.}
#'   \item{type}{Standard error type.}
#'   \item{gamman}{Asymptotic covariance matrix of the sample covariance matrix
#'     assuming multivariate normal distribution.}
#'   \item{gammahc}{Asymptotic covariance matrix HC adjustment.}
#'   \item{gamma}{Asymptotic covariance matrix of the sample covariance matrix.}
#'   \item{acov}{Asymptotic covariance matrix of the standardized slopes.}
#'   \item{vcov}{Sampling covariance matrix of the standardized slopes.}
#'   \item{est}{Vector of standardized slopes.}
#' }
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
#' @param g1 Numeric.
#'   `g1` value for `type = "hc4m"` or `type = "hc5"`.
#' @param g2 Numeric.
#'   `g2` value for `type = "hc4m"`.
#' @param k Numeric.
#'   Constant for `type = "hc5"`
#'
#' @references
#' Dudgeon, P. (2017).
#' Some improvements in confidence intervals
#' for standardized regression coefficients.
#' *Psychometrika*, *82*(4), 928–951.
#' \doi{10.1007/s11336-017-9563-z}
#'
#' @examples
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' std <- BetaHC(object)
#' # Methods ----------------------------------------------------
#' print(std)
#' summary(std)
#' coef(std)
#' vcov(std)
#' confint(std, level = 0.95)
#' ## Differences of standardized regression coefficients -------
#' out <- dif(std)
#' print(out)
#' summary(out)
#' coef(out)
#' vcov(out)
#' confint(out, level = 0.95)
#' ## Multiple Correlation --------------------------------------
#' out <- rsq(std)
#' print(out)
#' summary(out)
#' coef(out)
#' vcov(out)
#' confint(out, level = 0.95)
#' @export
#' @family Beta Sandwich Functions
#' @keywords betaSandwich
BetaHC <- function(object,
                   type = "hc3",
                   g1 = 1,
                   g2 = 1.5,
                   k = 0.7) {
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
    lm = object,
    lm_process = lm_process,
    type = type,
    gamman = gammacap_mvn,
    gammahc = gammacap,
    gamma = (
      gammacap_mvn %*% (
        wcap %*% gammacap %*% wcap
      ) %*% gammacap_mvn
    ),
    acov = chol2inv(chol(acov)),
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
