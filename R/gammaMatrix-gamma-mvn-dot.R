.GammaN <- function(sigmacap,
                    pinv_of_dcap) {
  2 * pinv_of_dcap %*% (
    tcrossprod(
      kronecker(
        sigmacap,
        sigmacap
      ),
      pinv_of_dcap
    )
  )
}
