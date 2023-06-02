#' Estimate Differences of Standardized Slopes
#' and the Corresponding Sampling Covariance Matrix
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns an object of class `diffbetasandwich`
#'   which is a list with the following elements:
#'   \describe{
#'     \item{call}{Function call.}
#'     \item{fit}{The argument `object`.}
#'     \item{args}{Function arguments.}
#'     \item{vcov}{Sampling covariance matrix of
#'       differences of standardized slopes.}
#'     \item{est}{Vector of
#'       differences of standardized slopes.}
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
#' diff <- DiffBetaSandwich(std)
#' # Methods -------------------------------------------------------
#' print(diff)
#' summary(diff)
#' coef(diff)
#' vcov(diff)
#' confint(diff, level = 0.95)
#'
#' @family Beta Sandwich Functions
#' @keywords betaSandwich diff
#' @export
DiffBetaSandwich <- function(object,
                             alpha = c(0.05, 0.01, 0.001)) {
  stopifnot(
    inherits(
      object,
      "betasandwich"
    )
  )
  if (object$lm_process$p < 2) {
    stop("Two or more regressors is required.")
  }
  est <- object$lm_process$dif_betastar
  jcap <- .JacobianDiffBetaStar(
    p = object$lm_process$p
  )
  vcov <- object$vcov[
    seq_len(object$lm_process$p),
    seq_len(object$lm_process$p),
    drop = FALSE
  ]
  vcov <- jcap %*% tcrossprod(
    vcov,
    jcap
  )
  colnames(vcov) <- rownames(vcov) <- names(object$lm_process$dif_betastar)
  out <- list(
    call = match.call(),
    fit = object,
    args = list(
      object = object,
      alpha = alpha
    ),
    vcov = vcov,
    est = est
  )
  class(out) <- c(
    "diffbetasandwich",
    class(out)
  )
  return(
    out
  )
}
