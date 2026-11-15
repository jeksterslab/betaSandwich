#' Jacobian of the Semipartial Correlations
#' with Respect to the Standardized Parameter Vector
#'
#' Jacobian of
#' \eqn{sr_{j} = \beta^{\ast}_{j} / \sqrt{d_{j}}},
#' where \eqn{d_{j}} is the \eqn{j}th diagonal element of
#' \eqn{\boldsymbol{R}_{\boldsymbol{X}}^{-1}},
#' with respect to
#' \eqn{\boldsymbol{\theta}^{\ast} = \left( \boldsymbol{\beta}^{\ast},
#' \sigma_{y}, \boldsymbol{\sigma}_{\boldsymbol{x}},
#' \operatorname{vechs} \left( \boldsymbol{R}_{\boldsymbol{X}} \right) \right)}
#' (`.ThetaStarIndex()`, without the means).
#' For \eqn{\rho_{ab}},
#' \eqn{\partial sr_{j} / \partial \rho_{ab} =
#' \beta^{\ast}_{j} s_{ja} s_{jb} / d_{j}^{3 / 2}},
#' with \eqn{s_{ja}} the elements of
#' \eqn{\boldsymbol{R}_{\boldsymbol{X}}^{-1}}.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams .JacRSqWRTThetaStar
#' @return Returns a `p` by `q` numeric matrix,
#'   where \eqn{q = 2p + 1 + p \left( p - 1 \right) / 2}.
#'
#' @examples
#' rhocapx <- matrix(data = c(1, 0.3, 0.3, 1), nrow = 2)
#' strRegression:::.JacSCorWRTThetaStar(
#'   betastar = c(0.4, 0.3),
#'   rhocapx = rhocapx
#' )
#'
#' @family Structure of Regression Functions
#' @keywords strRegression internal
#' @noRd
.JacSCorWRTThetaStar <- function(betastar,
                                 rhocapx) {
  p <- length(x = betastar)
  lower <- which(
    x = lower.tri(x = rhocapx),
    arr.ind = TRUE
  )
  scap <- chol2inv(
    x = chol(x = rhocapx)
  )
  d <- diag(x = scap)
  out <- matrix(
    data = 0.0,
    nrow = p,
    ncol = 2 * p + 1 + nrow(x = lower)
  )
  out[, seq_len(length.out = p)] <- diag(
    x = 1 / sqrt(x = d),
    nrow = p
  )
  if (p > 1) {
    out[, 2 * p + 1 + seq_len(length.out = nrow(x = lower))] <- (
      betastar / d^(3 / 2)
    ) * (
      scap[, lower[, 1], drop = FALSE] * scap[, lower[, 2], drop = FALSE]
    )
  }
  out
}
