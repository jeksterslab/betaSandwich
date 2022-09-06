#' Strict Half-Vectorize a Matrix
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Matrix.
#'
#' @family Vectorization Functions
#' @keywords linearAlgebra vectorization dot internal
.Vechs <- function(x) {
  x[
    lower.tri(
      x = x,
      diag = FALSE
    )
  ]
}
