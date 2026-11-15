#' Probabilities of Two-Sided Significance Levels (Internal)
#'
#' Returns the lower and upper tail probabilities
#' \eqn{\alpha / 2} and \eqn{1 - \alpha / 2}
#' of each two-sided significance level \eqn{\alpha},
#' sorted in increasing order.
#' No input checks: see `ProbsofAlpha()`.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param alpha Numeric vector.
#'   Significance level/s.
#'
#' @return Returns a numeric vector of probabilities
#'   of length `2 * length(alpha)`.
#'
#' @examples
#' .ProbsofAlpha(alpha = c(0.05, 0.01, 0.001))
#'
#' @family Wald Confidence Intervals Functions
#' @keywords wald alpha internal
#' @noRd
.ProbsofAlpha <- function(alpha = c(0.05, 0.01, 0.001)) {
  lower <- 0.5 * alpha
  sort(
    x = c(
      lower,
      1 - lower
    )
  )
}
