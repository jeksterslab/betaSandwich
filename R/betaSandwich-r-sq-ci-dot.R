#' Confidence Intervals for
#' Multiple Correlation Coefficients
#' (R-Squared and Adjusted R-Squared)
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a matrix of
#'   multiple correlation coefficients
#'   (R-squared and adjusted R-squared),
#'   standard errors,
#'   test statistics,
#'   degrees of freedom,
#'   p-values,
#'   and
#'   confidence intervals.
#'
#' @param object Object of class `rsqbetasandwich`.
#' @param alpha Numeric vector.
#'   Significance level.
#'
#' @family Beta Sandwich Functions
#' @keywords betaSandwich ci rsq internal
#' @noRd
.RSqCI <- function(object,
                   alpha = c(0.05, 0.01, 0.001)) {
  stopifnot(
    inherits(
      object,
      "rsqbetasandwich"
    )
  )
  return(
    .CIWald(
      est = object$fit$lm_process$rsq,
      se = sqrt(diag(.RSqCov(object))),
      theta = 0,
      alpha = alpha,
      z = FALSE,
      df = object$fit$lm_process$df
    )
  )
}
