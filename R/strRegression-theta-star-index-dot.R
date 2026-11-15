#' Create Index for the Standardized Parameter Vector
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param p Positive integer.
#'   `p` regressors.
#'
#' @return Returns a list of indices.
#' @examples
#' strRegression:::.ThetaStarIndex(p = 2)
#'
#' @family Standardized Parameters Functions
#' @keywords strRegression parametersstd internal
#' @noRd
.ThetaStarIndex <- function(p) {
  if (p == 1) {
    out <- list(
      betastar = paste0(
        "betastar",
        seq_len(length.out = p)
      ),
      sigmay = "sigmay",
      sigmax = paste0(
        "sigmax",
        seq_len(length.out = p)
      ),
      sigmastarsq = "sigmastarsq",
      muy = "muy",
      mux = paste0(
        "mux",
        seq_len(length.out = p)
      )
    )
  } else {
    out <- list(
      betastar = paste0(
        "betastar",
        seq_len(length.out = p)
      ),
      sigmay = "sigmay",
      sigmax = paste0(
        "sigmax",
        seq_len(length.out = p)
      ),
      vechsrhocapx = paste0(
        "rho",
        .VechsNames(
          x = paste0("x", seq_len(length.out = p)),
          sep = ""
        )
      ),
      sigmastarsq = "sigmastarsq",
      muy = "muy",
      mux = paste0(
        "mux",
        seq_len(length.out = p)
      )
    )
  }
  out
}
