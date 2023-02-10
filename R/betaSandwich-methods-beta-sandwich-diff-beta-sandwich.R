#' @rdname dif
#' @method dif betasandwich
#' @export
dif.betasandwich <- function(object, # nolint: object_name_linter
                             ...) {
  if (object$lm_process$p < 2) {
    stop("Two or more regressors is required.")
  }
  est <- object$lm_process$dif_betastar
  jcap <- .JacobianDiffBetastar(
    p = object$lm_process$p
  )
  vcov <- object$vcov[
    seq_len(object$lm_process$p),
    seq_len(object$lm_process$p),
    drop = FALSE
  ]
  vcov <- jcap %*% tcrossprod(
    vcov,
    jcap
  )
  colnames(vcov) <- rownames(vcov) <- names(object$lm_process$dif_betastar)
  out <- list(
    fit = object,
    vcov = vcov,
    est = est
  )
  class(out) <- c(
    "difbetasandwich",
    class(out)
  )
  return(
    out
  )
}
