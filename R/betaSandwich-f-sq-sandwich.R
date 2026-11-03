#' Estimate Cohen's f-Squared and Local f-Squared and the Corresponding Sampling
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
#'     \item{vcov}{Sampling covariance matrix of Cohen's f-squared (fsq) and
#'       the local f-squared of each regressor.}
#'     \item{est}{Vector of Cohen's f-squared (fsq) and the local f-squared of
#'       each regressor.}
#'     \item{fun}{Function used ("FSqSandwich").}
#'   }
#'
#' @inheritParams RSqSandwich
#'
#' @examples
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' std <- BetaHC(object)
#' out <- FSqSandwich(std)
#' # Methods -------------------------------------------------------
#' print(out)
#' summary(out)
#' coef(out)
#' vcov(out)
#' confint(out, level = 0.95)
#'
#' @family Beta Sandwich Functions
#' @keywords betaSandwich fsq
#' @export
FSqSandwich <- function(object,
                        alpha = c(0.05, 0.01, 0.001)) {
  .CheckSandwich(object = object)
  lm_process <- object$lm_process
  rhocapx <- lm_process$rhocap[
    2:lm_process$k,
    2:lm_process$k,
    drop = FALSE
  ]
  rsq <- lm_process$rsq[1]
  srsq <- lm_process$betastar^2 / diag(
    chol2inv(
      chol(rhocapx)
    )
  )
  est <- c(
    rsq / (1 - rsq),
    srsq / (1 - rsq)
  )
  names(est) <- c(
    "fsq",
    lm_process$xnames
  )
  .ESSandwich(
    object = object,
    jcap = .JacFSqWRTThetaStar(
      betastar = lm_process$betastar,
      rhocapx = rhocapx
    ),
    est = est,
    alpha = alpha,
    fun = "FSqSandwich",
    call = match.call()
  )
}
