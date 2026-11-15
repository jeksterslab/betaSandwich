#' Jacobian of Cohen's f-Squared
#' with Respect to the Standardized Parameter Vector
#'
#' Jacobian of the global
#' \eqn{f^{2} = R^{2} / \left( 1 - R^{2} \right)}
#' and the local
#' \eqn{f^{2}_{j} = sr^{2}_{j} / \left( 1 - R^{2} \right)}
#' (the order of `.FSq()`)
#' with respect to
#' \eqn{\boldsymbol{\theta}^{\ast}}
#' (`.ThetaStarIndex()`, without the means).
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams .JacRSqWRTThetaStar
#' @return Returns a `p + 1` by `q` numeric matrix,
#'   where \eqn{q = 2p + 1 + p \left( p - 1 \right) / 2}:
#'   the global \eqn{f^{2}}, then one row per regressor.
#'
#' @examples
#' rhocapx <- matrix(data = c(1, 0.3, 0.3, 1), nrow = 2)
#' strRegression:::.JacFSqWRTThetaStar(
#'   betastar = c(0.4, 0.3),
#'   rhocapx = rhocapx
#' )
#'
#' @family Structure of Regression Functions
#' @keywords strRegression internal
#' @noRd
.JacFSqWRTThetaStar <- function(betastar,
                                rhocapx) {
  rsq <- as.vector(
    x = crossprod(x = betastar, y = rhocapx %*% betastar)
  )
  srsq <- betastar^2 / diag(
    x = chol2inv(
      x = chol(x = rhocapx)
    )
  )
  jcap_rsq <- .JacRSqWRTThetaStar(
    betastar = betastar,
    rhocapx = rhocapx
  )
  rbind(
    jcap_rsq / (1 - rsq)^2,
    .JacDeltaRSqWRTThetaStar(
      betastar = betastar,
      rhocapx = rhocapx
    ) / (1 - rsq) + (srsq %*% jcap_rsq) / (1 - rsq)^2
  )
}
