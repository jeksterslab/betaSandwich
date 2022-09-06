#' Robust Confidence Intervals for Standardized Regression Coefficients
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a matrix.
#'
#' @param object Object of class `betaSandwich`.
#'
#' @family Beta Sandwich Functions
#' @keywords betaSandwich ci dot internal
.BetaCI <- function(object,
                    alpha = c(0.05, 0.01, 0.001)) {
  stopifnot(methods::is(object, "betaSandwich"))
  .CIWald(
    object$beta,
    se = sqrt(diag(object$beta.vcov)),
    theta = 0,
    alpha = alpha,
    z = FALSE,
    df = object$df
  )
}
