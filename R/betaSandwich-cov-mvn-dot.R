#' Sampling Covariance Matrix of the Standardized Parameter Vector
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param acov Numeric matrix.
#'   Asymptotic covariance matrix of the standardized parameter vector.
#' @param n Integer.
#'   Sample size.
#'
#' @family Beta Sandwich Functions
#' @keywords betaSandwich cov internal
#' @noRd
.CovN <- function(acov,
                  n) {
  return(
    (
      1 / n
    ) * chol2inv(
      chol(acov)
    )
  )
}
