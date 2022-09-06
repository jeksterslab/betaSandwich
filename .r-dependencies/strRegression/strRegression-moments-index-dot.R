#' Create Index for Moments Vector
#'
#' @param p Positive integer.
#'   `p` regressors.
#'
#' @family Moments Functions
#' @keywords strRegression moments dot internal
.MomentsIndex <- function(p) {
  list(
    sigmaysq = "sigmaysq",
    sigmayx = paste0(
      "sigmayx",
      seq_len(p)
    ),
    vechsigmacapx = paste0(
      "sigma",
      .VechNames(
        x = paste0("x", seq_len(p)),
        sep = ""
      )
    ),
    muy = "muy",
    mux = paste0(
      "mux",
      seq_len(p)
    )
  )
}
