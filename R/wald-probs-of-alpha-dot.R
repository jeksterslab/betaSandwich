#' Probabilities
#'
#' Generates a vector of probabilities
#' associated with the two-tailed `alpha` level provided in increasing order.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a vector of probabilities.
#'
#' @param alpha Numeric vector.
#'   Significance level/s.
#'
#' @family Wald Confidence Intervals Functions
#' @keywords alpha internal
#' @noRd
.ProbsofAlpha <- function(alpha = c(
                            0.05,
                            0.01,
                            0.001
                          )) {
  lower <- 0.5 * alpha
  return(
    sort(
      c(
        lower,
        1 - lower
      )
    )
  )
}
