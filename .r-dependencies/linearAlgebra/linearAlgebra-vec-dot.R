#' Vectorize a Matrix
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a vector.
#'
#' @param x Matrix.
#'
#' @family Vectorization Functions
#' @keywords linearAlgebra vectorization dot internal
.Vec <- function(x) {
  dim(x) <- NULL
  x
}
