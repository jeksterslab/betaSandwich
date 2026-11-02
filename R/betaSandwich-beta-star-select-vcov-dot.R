#' Sampling Covariance Matrix of Selectively Standardized Slopes
#'
#' Takes the sampling covariance matrix of the standardized parameters
#' (`.ThetaStarIndex()` order) and returns the block of the slopes,
#' or, when not every variable is standardized,
#' the delta-method covariance matrix of `.BetaStarSelect()`.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param vcov Numeric matrix.
#'   Sampling covariance matrix of the standardized parameters.
#' @param lm_process List.
#'   Output of `.ProcessLM()`.
#' @param std_y Logical.
#'   Standardize \eqn{y}.
#' @param std_x Logical vector.
#'   Standardize each regressor.
#' @return Returns a `p` by `p` numeric matrix.
#'
#' @family Beta Sandwich Functions
#' @keywords betaSandwich internal
#' @noRd
.BetaStarSelectVCov <- function(vcov,
                                lm_process,
                                std_y = TRUE,
                                std_x = rep(x = TRUE, times = lm_process$p)) {
  p <- lm_process$p
  if (std_y && all(std_x)) {
    out <- vcov[
      seq_len(p),
      seq_len(p),
      drop = FALSE
    ]
  } else {
    idx <- seq_len(2 * p + 1)
    jcap <- .JacobianBetaStarSelectWRTThetaStar(
      betastar = lm_process$betastar,
      sigmay = lm_process$sigma[1],
      sigmax = lm_process$sigma[-1],
      std_y = std_y,
      std_x = std_x
    )
    out <- jcap %*% tcrossprod(
      vcov[idx, idx, drop = FALSE],
      jcap
    )
  }
  colnames(out) <- rownames(out) <- lm_process$xnames
  out
}
