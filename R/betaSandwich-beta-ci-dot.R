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
#'   Significance level.
#'
#' @family Beta Sandwich Functions
#' @keywords betaSandwich ci internal
#' @noRd
.BetaCI <- function(object,
                    alpha = c(0.05, 0.01, 0.001)) {
  stopifnot(
    inherits(
      object,
      "betasandwich"
    )
  )
  return(
    .CIWald(
      est = object$est,
      se = sqrt(diag(object$vcov)),
      theta = 0,
      alpha = alpha,
      z = FALSE,
      df = object$lm_process$df
    )
  )
}
