#' Jacobian of the Selectively Standardized Partial Regression Slopes
#'
#' Jacobian of `.BetaStarSelect()` with respect to
#' \eqn{\left( \boldsymbol{\beta}^{\ast}, \sigma_{y},
#' \boldsymbol{\sigma}_{\mathbf{x}} \right)},
#' the first \eqn{2p + 1} elements of the standardized parameters
#' (`.ThetaStarIndex()`).
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams .BetaStarSelect
#' @param betastar Numeric vector of length `p`.
#' @param sigmay Numeric.
#' @param sigmax Numeric vector of length `p`.
#' @return Returns a `p` by `2 * p + 1` numeric matrix.
#'
#' @family Structure of Regression Functions
#' @keywords strRegression internal
#' @noRd
.JacobianBetaStarSelectWRTThetaStar <- function(betastar,
                                                sigmay,
                                                sigmax,
                                                std_y = TRUE,
                                                std_x) {
  p <- length(betastar)
  scale <- sigmay^(1 - std_y) * sigmax^(std_x - 1)
  jcap <- matrix(
    data = 0.0,
    nrow = p,
    ncol = 2 * p + 1
  )
  jcap[, seq_len(p)] <- diag(x = scale, nrow = p)
  jcap[, p + 1] <- betastar * (1 - std_y) * sigmay^(-std_y) *
    sigmax^(std_x - 1)
  jcap[, p + 1 + seq_len(p)] <- diag(
    x = betastar * sigmay^(1 - std_y) * (std_x - 1) * sigmax^(std_x - 2),
    nrow = p
  )
  jcap
}
