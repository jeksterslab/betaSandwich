#' Estimate Standardized Regression Coefficients
#' and the Corresponding Sampling Covariance Matrix
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
#' Browne, M. W. (1984).
#' Asymptotically distribution-free methods
#' for the analysis of covariance structures.
#' *British Journal of Mathematical and Statistical Psychology*,
#' *37*(1), 62–83.
#' \doi{10.1111/j.2044-8317.1984.tb00789.x}
#'
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
#' std <- BetaADF(object)
#' # Methods -------------------------------------------------------
#' print(std)
#' summary(std)
#' coef(std)
#' vcov(std)
#' confint(std, level = 0.95)
#' @export
#' @family Beta Sandwich Functions
#' @keywords betaSandwich std
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
  acov <- chol2inv(
    chol(
      .ACovSEMInverse(
        jcap = jcap,
        acov = gammacap_adf
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
    gamma_n = .GammaN(
      sigmacap = lm_process$sigmacap,
      pinv_of_dcap = .PInvDmat(.DMat(lm_process$k))
    ),
    gamma_hc = NULL,
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
