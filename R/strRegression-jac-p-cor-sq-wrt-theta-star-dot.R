#' Jacobian of the Squared Partial Correlations
#' with Respect to the Standardized Parameter Vector
#'
#' Jacobian of
#' \eqn{pr^{2}_{j} = sr^{2}_{j} / \left( 1 - R^{2} + sr^{2}_{j} \right)}
#' with respect to
#' \eqn{\boldsymbol{\theta}^{\ast}}
#' (`.ThetaStarIndex()`, without the means):
#' \eqn{\left[ \left( 1 - R^{2} \right) \partial sr^{2}_{j}
#' + sr^{2}_{j} \partial R^{2} \right]
#' / \left( 1 - R^{2} + sr^{2}_{j} \right)^{2}}.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams .JacRSqWRTThetaStar
#' @return Returns a `p` by `q` numeric matrix,
#'   where \eqn{q = 2p + 1 + p \left( p - 1 \right) / 2}.
#'
#' @family Structure of Regression Functions
#' @keywords strRegression internal
#' @noRd
.JacPCorSqWRTThetaStar <- function(betastar,
                                   rhocapx) {
  rsq <- as.vector(
    crossprod(betastar, rhocapx %*% betastar)
  )
  srsq <- betastar^2 / diag(
    chol2inv(
      chol(rhocapx)
    )
  )
  jcap_srsq <- .JacDeltaRSqWRTThetaStar(
    betastar = betastar,
    rhocapx = rhocapx
  )
  jcap_rsq <- .JacRSqWRTThetaStar(
    betastar = betastar,
    rhocapx = rhocapx
  )
  (
    (1 - rsq) * jcap_srsq + srsq %*% jcap_rsq
  ) / (1 - rsq + srsq)^2
}
