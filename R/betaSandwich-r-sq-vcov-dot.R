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
    methods::is(
      object,
      "rsqbetasandwich"
    )
  )
  vcov <- object$vcov[
    object$fit$lm_process$k,
    object$fit$lm_process$k,
    drop = FALSE
  ]
  jcap <- c(
    1,
    (
      -1 * (
        1 - object$fit$lm_process$n
      ) / (
        object$fit$lm_process$n - object$fit$lm_process$k
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
