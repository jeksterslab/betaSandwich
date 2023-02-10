#' Estimate Multiple Correlation Coefficients
#' (R-squared and adjusted R-squared)
#' and Sampling Covariance Matrix
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns an object of class `rsqbetasandwich`
#'   which is a list with the following elements:
#'   \describe{
#'     \item{fit}{The argument `object`.}
#'     \item{vcov}{Sampling covariance matrix of
#'       multiple correlation coefficients
#'       (R-squared and adjusted R-squared).}
#'     \item{est}{Vector of multiple correlation coefficients
#'       (R-squared and adjusted R-squared).}
#'   }
#'
#' @param object Object of class `betasandwich`,
#'   that is,
#'   the output of the `BetaHC()`, `BetaN()`, or `BetaADF()` functions.
#'
#' @examples
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' std <- BetaHC(object)
#' rsq <- RSqBetaSandwich(std)
#' # Methods -------------------------------------------------------
#' print(rsq)
#' summary(rsq)
#' coef(rsq)
#' vcov(rsq)
#' confint(rsq, level = 0.95)
#' @family Beta Sandwich Functions
#' @keywords betaSandwich rsq
#' @export
RSqBetaSandwich <- function(object) {
  stopifnot(
    methods::is(
      object,
      "betasandwich"
    )
  )
  est <- c(
    object$lm_process$beta,
    object$lm_process$summary_lm$r.squared,
    .Vech(
      object$lm_process$sigmacap[
        2:object$lm_process$k,
        2:object$lm_process$k,
        drop = FALSE
      ]
    )
  )
  jcap <- .JacobianVechSigmaWRTTheta(
    beta = object$lm_process$beta,
    sigmacapx = object$lm_process$sigmacap[
      2:object$lm_process$k,
      2:object$lm_process$k,
      drop = FALSE
    ],
    q = object$lm_process$q,
    p = object$lm_process$p,
    rsq = object$lm_process$summary_lm$r.squared
  )
  if (object$type %in% c("mvn", "adf")) {
    acov <- .ACovSEM(
      jcap = jcap,
      acov = object$gamma
    )
    vcov <- .CovN(
      acov = acov,
      n = object$lm_process$n
    )
  } else {
    acov <- .ACovHC(
      jcap = jcap,
      gammacap = object$gammahc,
      gammacap_mvn = object$gamman
    )
    vcov <- .CovHC(
      acov = acov,
      type = object$type,
      n = object$lm_process$n,
      df = object$lm_process$df
    )
  }
  out <- list(
    fit = object,
    vcov = vcov,
    est = est
  )
  class(out) <- c(
    "rsqbetasandwich",
    class(out)
  )
  return(
    out
  )
}
