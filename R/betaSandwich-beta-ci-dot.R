#' Confidence Intervals for
#' Standardized Regression Coefficients
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a matrix of
#'   standardized regression slopes,
#'   standard errors,
#'   test statistics,
#'   degrees of freedom,
#'   p-values,
#'   and
#'   confidence intervals.
#'
#' @param object Object of class `betasandwich`.
#' @param alpha Numeric vector.
#'   Significance level \eqn{\alpha}.
#'
#' @family Beta Sandwich Functions
#' @keywords betaSandwich ci internal
#' @noRd
.BetaCI <- function(object,
                    alpha = NULL) {
  if (is.null(alpha)) {
    alpha <- object$args$alpha
  }
  stopifnot(
    all(alpha > 0 & alpha < 1)
  )
  .CIWald(
    est = object$est,
    se = sqrt(diag(object$vcov)),
    theta = 0,
    alpha = alpha,
    z = FALSE,
    df = object$lm_process$df
  )
}
