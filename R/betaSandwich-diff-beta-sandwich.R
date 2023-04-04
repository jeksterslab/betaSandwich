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
#'     \item{vcov}{Sampling covariance matrix of
#'       differences of standardized slopes.}
#'     \item{est}{Vector of
#'       differences of standardized slopes.}
#'   }
#'
#' @param object Object of class `betasandwich`,
#'   that is,
#'   the output of the `BetaHC()`, `BetaN()`, or `BetaADF()` functions.
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
#' @export
#' @family Beta Sandwich Functions
#' @keywords betaSandwich diff
DiffBetaSandwich <- function(object) {
  stopifnot(
    methods::is(
      object,
      "betasandwich"
    )
  )
  if (object$lm_process$p < 2) {
    stop("Two or more regressors is required.")
  }
  est <- object$lm_process$dif_betastar
  jcap <- .JacobianDiffBetastar(
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
