#' Check the Input of the Effect-Size Sandwich Functions
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object Object of class `betasandwich`.
#' @param p_min Positive integer.
#'   Minimum number of regressors.
#'
#' @family Beta Sandwich Functions
#' @keywords betaSandwich internal
#' @noRd
.CheckSandwich <- function(object,
                           p_min = 1) {
  stopifnot(
    inherits(
      x = object,
      what = "betasandwich"
    )
  )
  if (object$lm_process$p < p_min) {
    stop("Two or more regressors is required.")
  }
  invisible(TRUE)
}
