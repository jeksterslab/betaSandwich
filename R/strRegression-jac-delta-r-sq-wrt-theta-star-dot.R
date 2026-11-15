#' Jacobian of the Improvement in R-Squared
#' with Respect to the Standardized Parameter Vector
#'
#' Jacobian of
#' \eqn{\Delta R^{2}_{j} = sr^{2}_{j}}
#' with respect to
#' \eqn{\boldsymbol{\theta}^{\ast}}
#' (`.ThetaStarIndex()`, without the means):
#' \eqn{2 sr_{j}} times the rows of `.JacSCorWRTThetaStar()`.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams .JacRSqWRTThetaStar
#' @return Returns a `p` by `q` numeric matrix,
#'   where \eqn{q = 2p + 1 + p \left( p - 1 \right) / 2}.
#'
#' @examples
#' rhocapx <- matrix(data = c(1, 0.3, 0.3, 1), nrow = 2)
#' strRegression:::.JacDeltaRSqWRTThetaStar(
#'   betastar = c(0.4, 0.3),
#'   rhocapx = rhocapx
#' )
#'
#' @family Structure of Regression Functions
#' @keywords strRegression internal
#' @noRd
.JacDeltaRSqWRTThetaStar <- function(betastar,
                                     rhocapx) {
  scor <- betastar / sqrt(
    x = diag(
      x = chol2inv(
        x = chol(x = rhocapx)
      )
    )
  )
  2 * scor * .JacSCorWRTThetaStar(
    betastar = betastar,
    rhocapx = rhocapx
  )
}
