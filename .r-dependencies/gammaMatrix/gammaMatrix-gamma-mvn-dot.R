#' Asymptotic Covariance Matrix of the Sample Covariance Matrix
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param sigmacap Numeric matrix.
#'   Covariance matrix.
#' @param pinv_of_dcap Numeric matrix.
#'   Moore-Penrose inverse of the duplication matrix.
#' @family gammaMatrix Functions
#' @keywords gammaMatrix gamma internal
#' @noRd
.GammaN <- function(sigmacap,
                    pinv_of_dcap) {
  return(
    2 * pinv_of_dcap %*% (
      tcrossprod(
        kronecker(
          sigmacap,
          sigmacap
        ),
        pinv_of_dcap
      )
    )
  )
}
