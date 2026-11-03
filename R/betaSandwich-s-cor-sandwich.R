#' Estimate Semipartial Correlation Coefficients and the Corresponding Sampling
#' Covariance Matrix
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @details The sampling covariance matrix is obtained by the delta method
#'   from the
#'   sampling covariance matrix of the standardized parameters stored in
#'   `object` (normal-theory, asymptotically distribution-free, or
#'   heteroskedasticity-consistent). Two or more regressors are required.
#'
#' @return Returns an object of class `essandwich`
#'   which is a list with the following elements:
#'   \describe{
#'     \item{call}{Function call.}
#'     \item{fit}{The argument `object`.}
#'     \item{args}{Function arguments.}
#'     \item{vcov}{Sampling covariance matrix of semipartial correlation
#'       coefficients.}
#'     \item{est}{Vector of semipartial correlation coefficients.}
#'     \item{fun}{Function used ("SCorSandwich").}
#'   }
#'
#' @inheritParams RSqSandwich
#'
#' @examples
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' std <- BetaHC(object)
#' out <- SCorSandwich(std)
#' # Methods -------------------------------------------------------
#' print(out)
#' summary(out)
#' coef(out)
#' vcov(out)
#' confint(out, level = 0.95)
#'
#' @family Beta Sandwich Functions
#' @keywords betaSandwich scor
#' @export
SCorSandwich <- function(object,
                         alpha = c(0.05, 0.01, 0.001)) {
  .CheckSandwich(
    object = object,
    p_min = 2
  )
  lm_process <- object$lm_process
  .ESSandwich(
    object = object,
    jcap = .JacSCorWRTThetaStar(
      betastar = lm_process$betastar,
      rhocapx = lm_process$rhocap[
        2:lm_process$k,
        2:lm_process$k,
        drop = FALSE
      ]
    ),
    est = lm_process$scor,
    alpha = alpha,
    fun = "SCorSandwich",
    call = match.call()
  )
}
