#' Estimate Standardized Regression Coefficients
#' and Sampling Covariance Matrix
#' Using the Asymptotic Distribution-Free Approach
#'
#' @details
#' Note that while the calculation in `BetaADF()`
#' is different from `betaDelta::BetaDelta()` with `type = "adf"`,
#' the results are numerically equivalent.
#' `BetaADF()` is appropriate when sample sizes are moderate to large
#' (`n > 250`).
#' `BetaHC()` is recommended in most situations.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns an object of class `betasandwich`
#'   which is a list with the following elements:
#'   \describe{
#'     \item{call}{Function call.}
#'     \item{lm}{Object of class `lm`.}
#'     \item{lm_process}{Pre-processed object of class `lm`.}
#'     \item{type}{Standard error type.}
#'     \item{gamma}{Asymptotic covariance matrix
#'       of the sample covariance matrix.}
#'     \item{acov}{Asymptotic covariance matrix of the standardized slopes.}
#'     \item{vcov}{Sampling covariance matrix of the standardized slopes.}
#'     \item{est}{Vector of standardized slopes.}
#'   }
#'
#' @param object Object of class `lm`.
#'
#' @examples
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' std <- BetaADF(object)
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
BetaADF <- function(object) {
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
  sigmacap_consistent <- (
    lm_process$sigmacap * (
      lm_process$n - 1
    ) / lm_process$n
  )
  vechsigmacap_consistent <- .Vech(
    sigmacap_consistent
  )
  gammacap_adf <- .GammaADFUnbiased(
    gammacapadf_consistent = .GammaADFConsistent(
      d = .DofMat(
        lm_process$x,
        center = colMeans(lm_process$x),
        n = lm_process$n,
        k = lm_process$k
      ),
      vechsigmacap_consistent = vechsigmacap_consistent,
      n = lm_process$n
    ),
    gammacapmvn_consistent = .GammaN(
      sigmacap = sigmacap_consistent,
      pinv_of_dcap = .PInvDmat(.DMat(lm_process$k))
    ),
    vechsigmacap_consistent = vechsigmacap_consistent,
    n = lm_process$n
  )
  # the procedure from here on is the same as normal
  acov <- .ACovSEM(
    jcap = jcap,
    acov = gammacap_adf
  )
  vcov <- .CovN(
    acov = acov,
    n = lm_process$n
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
    type = "adf",
    gamma = gammacap_adf,
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
