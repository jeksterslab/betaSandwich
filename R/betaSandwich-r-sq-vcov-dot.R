#' Sampling Variance-Covariance Matrix of
#' Multiple Correlation Coefficients
#' (R-Squared and Adjusted R-Squared)
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object Object of class `rsqbetasandwich`.
#'
#' @family Beta Sandwich Functions
#' @keywords betaSandwich vcov rsq internal
#' @noRd
.RSqCov <- function(object) {
  stopifnot(
    inherits(
      object,
      "rsqbetasandwich"
    )
  )
  vcov <- object$vcov[
    object$betasandwich$lm_process$k,
    object$betasandwich$lm_process$k,
    drop = FALSE
  ]
  jcap <- c(
    1,
    (
      -1 * (
        1 - object$betasandwich$lm_process$n
      ) / (
        object$betasandwich$lm_process$n - object$betasandwich$lm_process$k
      )
    )
  )
  vcov <- jcap %*% vcov %*% t(jcap)
  colnames(vcov) <- rownames(vcov) <- c(
    "rsq",
    "adj"
  )
  return(vcov)
}
