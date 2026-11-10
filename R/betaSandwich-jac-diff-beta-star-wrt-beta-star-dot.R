#' Jacobian Matrix of Differences of Standardized Regression Slopes
#' with Respect to the Standardized Regression Slopes
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param p positive integer.
#'   Number of regressors.
#'
#' @family Beta Sandwich Functions
#' @keywords betaSandwich derivatives internal
#' @noRd
.JacDiffBetaStar <- function(p) {
  idx <- utils::combn(x = seq_len(length.out = p), m = 2)
  q <- dim(x = idx)[2]
  out <- matrix(
    data = 0,
    nrow = q,
    ncol = p
  )
  for (i in seq_len(length.out = q)) {
    j <- idx[, i]
    out[i, j[1]] <- 1
    out[i, j[2]] <- -1
  }
  out
}
