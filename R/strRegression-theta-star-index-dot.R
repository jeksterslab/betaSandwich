#' Create Index for the Standardized Parameter Vector
#'
#' @param p Positive integer.
#'   `p` regressors.
#'
#' @return Returns a list of indices.
#' @family Standardized Parameters Functions
#' @keywords strRegression parametersstd internal
#' @noRd
.ThetaStarIndex <- function(p) {
  if (p == 1) {
    out <- list(
      betastar = paste0(
        "betastar",
        seq_len(p)
      ),
      sigmay = "sigmay",
      sigmax = paste0(
        "sigmax",
        seq_len(p)
      ),
      sigmastarsq = "sigmastarsq",
      muy = "muy",
      mux = paste0(
        "mux",
        seq_len(p)
      )
    )
  } else {
    out <- list(
      betastar = paste0(
        "betastar",
        seq_len(p)
      ),
      sigmay = "sigmay",
      sigmax = paste0(
        "sigmax",
        seq_len(p)
      ),
      vechsrhocapx = paste0(
        "rho",
        .VechsNames(
          x = paste0("x", seq_len(p)),
          sep = ""
        )
      ),
      sigmastarsq = "sigmastarsq",
      muy = "muy",
      mux = paste0(
        "mux",
        seq_len(p)
      )
    )
  }
  out
}
