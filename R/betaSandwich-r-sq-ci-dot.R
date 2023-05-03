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
      c(
        rsq = object$fit$lm_process$rsq[1],
        adj = object$fit$lm_process$rsq[2]
      ),
      se = sqrt(diag(.RSqCov(object))),
      theta = 0,
      alpha = alpha,
      z = FALSE,
      df = object$fit$lm_process$df
    )
  )
}
