#' Strict Half-Vectorize a Matrix
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Matrix.
#'
#' @return Returns a vector.
#'
#' @family Vectorization Functions
#' @keywords linearAlgebra vectorization internal
#' @noRd
.Vechs <- function(x) {
  x[
    lower.tri(
      x = x,
      diag = FALSE
    )
  ]
}
