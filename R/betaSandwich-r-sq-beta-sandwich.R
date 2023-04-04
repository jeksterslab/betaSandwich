#' Estimate Multiple Correlation Coefficients
#' (R-squared and adjusted R-squared)
#' and the Corresponding Sampling Covariance Matrix
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns an object of class `rsqbetasandwich`
#'   which is a list with the following elements:
#'   \describe{
#'     \item{call}{Function call.}
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
#' @export
#' @family Beta Sandwich Functions
#' @keywords betaSandwich rsq
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
  if (object$args$type %in% c("mvn", "adf")) {
    acov <- chol2inv(chol(.ACovSEMInverse(
      jcap = jcap,
      acov = object$gamma
    )))
    vcov <- (1 / object$lm_process$n) * acov
  } else {
    acov <- .ACovHC(
      jcap = jcap,
      gammacap = object$gamma_hc,
      gammacap_mvn = object$gamma_n
    )
    vcov <- .CovHC(
      acov = acov,
      type = object$args$type,
      n = object$lm_process$n,
      df = object$lm_process$df
    )
  }
  out <- list(
    call = match.call(),
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
