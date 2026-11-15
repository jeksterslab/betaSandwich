#' Strict Half-Vectorize a Matrix (Internal)
#'
#' Returns \eqn{\operatorname{vechs} \! \left( \boldsymbol{A} \right)},
#' the lower triangle of a square matrix, diagonal excluded,
#' stacked column by column.
#' No input checks: see `Vechs()`.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Square matrix.
#'
#' @return Returns a vector of length \eqn{k (k - 1) / 2}.
#'
#' @examples
#' .Vechs(x = toeplitz((3:1) / 3))
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
