#' Jacobian Matrix of the Half-Vectorization
#' of the Model-Implied Covariance Matrix
#' with Respect to the Standardized Parameter Vector
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param betastar Numeric vector.
#'   Standardized regression slopes.
#' @param sigmay Numeric.
#'   Standard deviation of the regressand variable.
#' @param sigmax Numeric vector.
#'   Standard deviations of the regressor variables.
#' @param rhocapx Numeric matrix.
#'   Correlation matrix of the regressor variables.
#' @param q Positive integer.
#'   Length of the parameter vector.
#' @param p Positive integer.
#'   `p` regressors.
#'
#' @return Returns a matrix.
#' @family Derivatives Functions
#' @keywords strRegression derivatives internal
#' @noRd
.JacobianVechSigmaWRTThetaStar <- function(betastar,
                                           sigmay,
                                           sigmax,
                                           rhocapx,
                                           q,
                                           p) {
  dsigmax <- diag(p)
  diag(dsigmax) <- sigmax
  dsigmax
  dp <- .DMat(p)
  iden <- diag(p)
  diags <- diag(
    matrix(
      data = seq_len(p * p),
      nrow = p,
      ncol = p
    )
  )
  vechnames_rhocapx <- paste0(
    "rho",
    .VechNames(
      x = paste0("x", seq_len(p)),
      sep = ""
    )
  )
  vecnames_rhocapx <- paste0(
    "rho",
    outer(
      X = seq_len(p),
      Y = seq_len(p),
      FUN = function(x, y) {
        paste0(
          "x",
          x,
          "x",
          y
        )
      }
    )
  )
  vecnames_sigmacapx <- paste0(
    "sigma",
    outer(
      X = seq_len(p),
      Y = seq_len(p),
      FUN = function(x, y) {
        paste0(
          "x",
          x,
          "x",
          y
        )
      }
    )
  )
  theta <- .ThetaStarIndex(
    p = p
  )
  moments <- .MomentsIndex(
    p = p
  )
  jcap <- matrix(
    data = 0,
    nrow = q,
    ncol = q
  )
  rownames(jcap) <- c(
    moments$sigmaysq,
    moments$sigmayx,
    moments$vechsigmacapx
  )
  if (p == 1) {
    colnames(jcap) <- c(
      theta$betastar,
      theta$sigmay,
      theta$sigmax
    )
  } else {
    colnames(jcap) <- c(
      theta$betastar,
      theta$sigmay,
      theta$sigmax,
      theta$vechsrhocapx
    )
  }
  jcap[
    moments$sigmaysq,
    theta$sigmay
  ] <- 2 * sigmay
  jcap[
    moments$sigmayx,
    theta$betastar
  ] <- sigmay * (dsigmax %*% rhocapx)
  jcap[
    moments$sigmayx,
    theta$sigmay
  ] <- (rhocapx %*% betastar) * sigmax
  sigmayx_wrt_sigmax <- .Vec(rhocapx %*% betastar)
  d_rhocapx_betastar <- diag(length(sigmayx_wrt_sigmax))
  diag(d_rhocapx_betastar) <- sigmayx_wrt_sigmax
  sigmayx_wrt_sigmax <- sigmay * d_rhocapx_betastar
  jcap[
    moments$sigmayx,
    theta$sigmax
  ] <- sigmayx_wrt_sigmax
  sigmayx_wrt_vechsrhocapx <- sigmay * kronecker(
    t(betastar),
    dsigmax
  ) %*% dp
  colnames(sigmayx_wrt_vechsrhocapx) <- vechnames_rhocapx
  jcap[
    moments$sigmayx,
    theta$vechsrhocapx
  ] <- sigmayx_wrt_vechsrhocapx[, theta$vechsrhocapx, drop = FALSE]
  vechsigmacapx_wrt_sigmax <- kronecker(
    t(
      rhocapx %*% dsigmax
    ),
    iden
  ) + kronecker(
    iden,
    dsigmax %*% rhocapx
  )
  vechsigmacapx_wrt_sigmax <- vechsigmacapx_wrt_sigmax[, diags, drop = FALSE]
  jcap[
    moments$vechsigmacapx,
    theta$sigmax
  ] <- do.call(
    what = "cbind",
    args = lapply(
      X = seq_len(p),
      FUN = function(i) {
        .Vech(
          matrix(
            data = vechsigmacapx_wrt_sigmax[, i],
            nrow = p
          )
        )
      }
    )
  )
  if (p > 1) {
    vechsigmacapx_wrt_vechsrhocapx <- kronecker(
      dsigmax,
      dsigmax
    )
    colnames(vechsigmacapx_wrt_vechsrhocapx) <- vecnames_rhocapx
    rownames(vechsigmacapx_wrt_vechsrhocapx) <- vecnames_sigmacapx
    vechsigmacapx_wrt_vechsrhocapx
    jcap[
      moments$vechsigmacapx,
      theta$vechsrhocapx
    ] <- vechsigmacapx_wrt_vechsrhocapx[
      moments$vechsigmacapx,
      theta$vechsrhocapx,
      drop = FALSE
    ]
  }
  return(
    jcap
  )
}
