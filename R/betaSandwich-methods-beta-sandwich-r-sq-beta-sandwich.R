#' @rdname rsq
#' @method rsq betasandwich
#' @export
rsq.betasandwich <- function(object, # nolint: object_name_linter
                             ...) {
  est <- c(
    object$lm_process$beta,
    object$lm_process$summary_lm$r.squared,
    .Vech(
      object$lm_process$sigmacap[
        2:object$lm_process$k,
        2:object$lm_process$k,
        drop = FALSE
      ]
    )
  )
  jcap <- .JacobianVechSigmaWRTTheta(
    beta = object$lm_process$beta,
    sigmacapx = object$lm_process$sigmacap[
      2:object$lm_process$k,
      2:object$lm_process$k,
      drop = FALSE
    ],
    q = object$lm_process$q,
    p = object$lm_process$p,
    rsq = object$lm_process$summary_lm$r.squared
  )
  if (object$type %in% c("mvn", "adf")) {
    acov <- .ACovSEM(
      jcap = jcap,
      acov = object$gamma
    )
    vcov <- .CovN(
      acov = acov,
      n = object$lm_process$n
    )
  } else {
    acov <- .ACovHC(
      jcap = jcap,
      gammacap = object$gammahc,
      gammacap_mvn = object$gamman
    )
    vcov <- .CovHC(
      acov = acov,
      type = object$type,
      n = object$lm_process$n,
      df = object$lm_process$df
    )
  }
  out <- list(
    fit = object,
    acov = acov,
    vcov = vcov,
    est = est
  )
  class(out) <- c(
    "rsqbetasandwich",
    class(out)
  )
  return(
    out
  )
}
