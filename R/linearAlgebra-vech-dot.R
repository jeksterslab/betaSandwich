#' Half-Vectorize a Matrix
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Matrix.
#'
#' @family Vectorization Functions
#' @keywords linearAlgebra vectorization dot internal
.Vech <- function(x) {
  x[
    lower.tri(
      x = x,
      diag = TRUE
    )
  ]
}
