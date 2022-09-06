#' The Moore-Penrose Inverse of the Duplication Matrix
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param d Numeric matrix.
#'   Duplication matrix.
#'
#' @family Symmetric Functions
#' @keywords linearAlgebra symmetric dot internal
.PInvDmat <- function(d) {
  tcrossprod(
    chol2inv(
      chol(
        crossprod(d)
      )
    ),
    d
  )
}
