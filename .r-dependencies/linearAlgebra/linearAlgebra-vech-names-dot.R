#' Vector Names for Half-Vectorization
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a vector of character stings.
#'
#' @param x Character vector of names of length `k`.
#' @param sep Character string.
#'   Separator for variable names.
#'
#' @family Vectorization Functions
#' @keywords linearAlgebra vectorization dot internal
.VechNames <- function(x,
                       sep = ".") {
  out <- outer(
    X = x,
    Y = x,
    FUN = function(x,
                   y) {
      paste0(
        x,
        sep,
        y
      )
    }
  )
  .Vech(out)
}
