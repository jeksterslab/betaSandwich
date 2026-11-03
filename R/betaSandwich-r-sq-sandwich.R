#' Estimate R-Squared and Adjusted R-Squared and the Corresponding Sampling
#' Covariance Matrix
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @details The sampling covariance matrix is obtained by the delta method
#'   from the
#'   sampling covariance matrix of the standardized parameters stored in
#'   `object` (normal-theory, asymptotically distribution-free, or
#'   heteroskedasticity-consistent).
#'
#' @return Returns an object of class `essandwich`
#'   which is a list with the following elements:
#'   \describe{
#'     \item{call}{Function call.}
#'     \item{fit}{The argument `object`.}
#'     \item{args}{Function arguments.}
#'     \item{vcov}{Sampling covariance matrix of R-squared and adjusted
#'       R-squared.}
#'     \item{est}{Vector of R-squared and adjusted R-squared.}
#'     \item{fun}{Function used ("RSqSandwich").}
#'   }
#'
#' @param object Object of class `betasandwich`,
#'   that is,
#'   the output of the [BetaHC()], [BetaN()], or [BetaADF()] functions.
#' @param alpha Numeric vector.
#'   Significance level \eqn{\alpha}.
#'
#' @examples
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' std <- BetaHC(object)
#' out <- RSqSandwich(std)
#' # Methods -------------------------------------------------------
#' print(out)
#' summary(out)
#' coef(out)
#' vcov(out)
#' confint(out, level = 0.95)
#'
#' @family Beta Sandwich Functions
#' @keywords betaSandwich rsq
#' @export
RSqSandwich <- function(object,
                        alpha = c(0.05, 0.01, 0.001)) {
  .CheckSandwich(object = object)
  lm_process <- object$lm_process
  jcap <- .JacRSqWRTThetaStar(
    betastar = lm_process$betastar,
    rhocapx = lm_process$rhocap[
      2:lm_process$k,
      2:lm_process$k,
      drop = FALSE
    ]
  )
  .ESSandwich(
    object = object,
    jcap = rbind(
      jcap,
      ((lm_process$n - 1) / (lm_process$n - lm_process$k)) * jcap
    ),
    est = lm_process$rsq,
    alpha = alpha,
    fun = "RSqSandwich",
    call = match.call()
  )
}
