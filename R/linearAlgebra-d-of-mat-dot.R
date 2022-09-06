#' Deviation from the Mean (Matrix Input)
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Numeric matrix.
#'   Data matrix.
#' @param center Numeric vector.
#'   Center.
#' @param n Positive integer.
#'   Number of rows in the data matrix `x`.
#' @param k Positive integer.
#'   Number of columns in the data matrix `x`.
#'
#' @return A matrix.
#'
#' @family Scaling Functions
#' @keywords linearAlgebra scaling dot internal
.DofMat <- function(x,
                    center,
                    n,
                    k) {
  x - rep(
    x = center,
    times = rep(
      x = n,
      times = k
    )
  )
}
