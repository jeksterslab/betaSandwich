#' Confidence Intervals for
#' Differences of Standardized Regression Slopes
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a matrix of
#'   differences of standardized regression slopes,
#'   standard errors,
#'   test statistics,
#'   p-values,
#'   and
#'   confidence intervals.
#'
#' @param object Object of class `difbetadelta`.
#' @param alpha Numeric vector.
#'   Significance level \eqn{\alpha}.
#'
#' @family Beta Sandwich Functions
#' @keywords betaSandwich dif ci internal
#' @noRd
.DiffBetaCI <- function(object,
                        alpha = NULL) {
  stopifnot(
    inherits(
      object,
      "diffbetasandwich"
    )
  )
  if (is.null(alpha)) {
    alpha <- object$args$alpha
  }
  stopifnot(
    all(alpha > 0 & alpha < 1)
  )
  return(
    .CIWald(
      est = object$est,
      se = sqrt(diag(object$vcov)),
      theta = 0,
      alpha = alpha,
      z = TRUE
    )
  )
}
