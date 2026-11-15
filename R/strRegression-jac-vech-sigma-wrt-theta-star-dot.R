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
#' @examples
#' rhocapx <- matrix(data = c(1, 0.3, 0.3, 1), nrow = 2)
#' strRegression:::.JacVechSigmaWRTThetaStar(
#'   betastar = c(0.4, 0.3),
#'   sigmay = 2,
#'   sigmax = c(1, 1.5),
#'   rhocapx = rhocapx,
#'   q = 6,
#'   p = 2
#' )
#'
#' @family Derivatives Functions
#' @keywords strRegression derivatives internal
#' @noRd
.JacVechSigmaWRTThetaStar <- function(betastar,
                                      sigmay,
                                      sigmax,
                                      rhocapx,
                                      q,
                                      p) {
  dsigmax <- diag(x = p)
  diag(x = dsigmax) <- sigmax
  dsigmax
  dp <- .DMat(k = p)
  iden <- diag(x = p)
  diags <- diag(
    x = matrix(
      data = seq_len(length.out = p * p),
      nrow = p,
      ncol = p
    )
  )
  vechnames_rhocapx <- paste0(
    "rho",
    .VechNames(
      x = paste0("x", seq_len(length.out = p)),
      sep = ""
    )
  )
  vecnames_rhocapx <- paste0(
    "rho",
    outer(
      X = seq_len(length.out = p),
      Y = seq_len(length.out = p),
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
      X = seq_len(length.out = p),
      Y = seq_len(length.out = p),
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
  rownames(x = jcap) <- c(
    moments$sigmaysq,
    moments$sigmayx,
    moments$vechsigmacapx
  )
  if (p == 1) {
    colnames(x = jcap) <- c(
      theta$betastar,
      theta$sigmay,
      theta$sigmax
    )
  } else {
    colnames(x = jcap) <- c(
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
  sigmayx_wrt_sigmax <- .Vec(x = rhocapx %*% betastar)
  d_rhocapx_betastar <- diag(x = length(x = sigmayx_wrt_sigmax))
  diag(x = d_rhocapx_betastar) <- sigmayx_wrt_sigmax
  sigmayx_wrt_sigmax <- sigmay * d_rhocapx_betastar
  jcap[
    moments$sigmayx,
    theta$sigmax
  ] <- sigmayx_wrt_sigmax
  sigmayx_wrt_vechsrhocapx <- sigmay * kronecker(
    X = t(x = betastar),
    Y = dsigmax
  ) %*% dp
  colnames(x = sigmayx_wrt_vechsrhocapx) <- vechnames_rhocapx
  jcap[
    moments$sigmayx,
    theta$vechsrhocapx
  ] <- sigmayx_wrt_vechsrhocapx[, theta$vechsrhocapx, drop = FALSE]
  vechsigmacapx_wrt_sigmax <- kronecker(
    X = t(
      x = rhocapx %*% dsigmax
    ),
    Y = iden
  ) + kronecker(
    X = iden,
    Y = dsigmax %*% rhocapx
  )
  vechsigmacapx_wrt_sigmax <- vechsigmacapx_wrt_sigmax[, diags, drop = FALSE]
  jcap[
    moments$vechsigmacapx,
    theta$sigmax
  ] <- do.call(
    what = "cbind",
    args = lapply(
      X = seq_len(length.out = p),
      FUN = function(i) {
        .Vech(
          x = matrix(
            data = vechsigmacapx_wrt_sigmax[, i],
            nrow = p
          )
        )
      }
    )
  )
  if (p > 1) {
    vechsigmacapx_wrt_vechsrhocapx <- kronecker(
      X = dsigmax,
      Y = dsigmax
    )
    colnames(x = vechsigmacapx_wrt_vechsrhocapx) <- vecnames_rhocapx
    rownames(x = vechsigmacapx_wrt_vechsrhocapx) <- vecnames_sigmacapx
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
  jcap
}
