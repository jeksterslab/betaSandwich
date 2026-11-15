#' Element Names for the Strict Half-Vectorization (Internal)
#'
#' Pastes each pair of names with `sep`, as `row`, `sep`, `column`,
#' and returns the pairs in the order of `.Vechs()`.
#' No input checks.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Character vector of length \eqn{k}.
#'   Variable names.
#' @param sep Character string.
#'   Separator for variable names.
#'
#' @return Returns a character vector of length \eqn{k (k - 1) / 2}.
#'
#' @examples
#' .VechsNames(x = c("x", "y", "z"), sep = ",")
#'
#' @family Vectorization Functions
#' @keywords linearAlgebra vectorization internal
#' @noRd
.VechsNames <- function(x,
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
  .Vechs(x = out)
}
