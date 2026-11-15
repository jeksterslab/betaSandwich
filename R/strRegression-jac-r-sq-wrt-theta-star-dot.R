#' Jacobian of the Coefficient of Determination
#' with Respect to the Standardized Parameter Vector
#'
#' Jacobian of
#' \eqn{R^2 = \boldsymbol{\beta}^{\ast \prime} \boldsymbol{R}_{\boldsymbol{X}}
#' \boldsymbol{\beta}^{\ast}}
#' with respect to
#' \eqn{\boldsymbol{\theta}^{\ast} = \left( \boldsymbol{\beta}^{\ast},
#' \sigma_{y}, \boldsymbol{\sigma}_{\boldsymbol{x}},
#' \operatorname{vechs} \left( \boldsymbol{R}_{\boldsymbol{X}} \right) \right)}
#' (`.ThetaStarIndex()`, without the means).
#' The Jacobian of the adjusted \eqn{R^2} is this row
#' times \eqn{\left( n - 1 \right) / \left( n - p - 1 \right)}.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param betastar Numeric vector of length `p`.
#'   Standardized regression slopes.
#' @param rhocapx Numeric matrix.
#'   `p` by `p` correlation matrix of the regressor variables.
#' @return Returns a `1` by `q` numeric matrix,
#'   where \eqn{q = 2p + 1 + p \left( p - 1 \right) / 2}.
#'
#' @examples
#' rhocapx <- matrix(data = c(1, 0.3, 0.3, 1), nrow = 2)
#' strRegression:::.JacRSqWRTThetaStar(
#'   betastar = c(0.4, 0.3),
#'   rhocapx = rhocapx
#' )
#'
#' @family Structure of Regression Functions
#' @keywords strRegression internal
#' @noRd
.JacRSqWRTThetaStar <- function(betastar,
                                rhocapx) {
  p <- length(x = betastar)
  lower <- which(
    x = lower.tri(x = rhocapx),
    arr.ind = TRUE
  )
  out <- matrix(
    data = 0.0,
    nrow = 1,
    ncol = 2 * p + 1 + nrow(x = lower)
  )
  out[1, seq_len(length.out = p)] <- 2 * as.vector(x = rhocapx %*% betastar)
  if (p > 1) {
    out[1, 2 * p + 1 + seq_len(length.out = nrow(x = lower))] <- 2 * betastar[
      lower[, 1]
    ] * betastar[
      lower[, 2]
    ]
  }
  out
}
