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
  gammacap <- object$acov[
    seq_len(object$lm_process$p),
    seq_len(object$lm_process$p),
    drop = FALSE
  ]
  acov <- jcap %*% tcrossprod(
    gammacap,
    jcap
  )
  colnames(acov) <- rownames(acov) <- names(object$lm_process$dif_betastar)
  vcov <- (1 / object$lm_process$n) * acov
  out <- list(
    fit = object,
    acov = acov,
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
