#' Sampling Covariance Matrix of an Effect Size
#' from a Fitted `betasandwich` Object
#'
#' Applies the Jacobian `jcap` of an effect size
#' with respect to the standardized parameter vector
#' \eqn{\boldsymbol{\theta}^{\ast} = \left( \boldsymbol{\beta}^{\ast},
#' \sigma_{y}, \boldsymbol{\sigma}_{\mathbf{x}},
#' \mathrm{vechs} \left( \mathbf{R}_{\mathbf{X}} \right) \right)}
#' to the sampling covariance matrix of
#' \eqn{\hat{\boldsymbol{\theta}}^{\ast}} stored in `object`
#' and returns an object of class `essandwich`.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object Object of class `betasandwich`.
#' @param jcap Numeric matrix.
#'   Jacobian of the effect size with respect to
#'   \eqn{\boldsymbol{\theta}^{\ast}}.
#' @param est Named numeric vector.
#'   Estimates of the effect size.
#' @param alpha Numeric vector.
#'   Significance level \eqn{\alpha}.
#' @param fun Character string.
#'   Name of the calling function.
#' @param call Function call.
#'
#' @family Beta Sandwich Functions
#' @keywords betaSandwich internal
#' @noRd
.ESSandwich <- function(object,
                        jcap,
                        est,
                        alpha,
                        fun,
                        call) {
  if (object$args$type %in% c("mvn", "adf")) {
    vcov <- (1 / object$lm_process$n) * object$acov
  } else {
    vcov <- .CovHC(
      acov = object$acov,
      type = object$args$type,
      n = object$lm_process$n,
      df = object$lm_process$df
    )
  }
  vcov <- jcap %*% tcrossprod(
    vcov,
    jcap
  )
  colnames(vcov) <- rownames(vcov) <- names(est)
  out <- list(
    call = call,
    fit = object,
    args = list(
      object = object,
      alpha = alpha
    ),
    vcov = vcov,
    est = est,
    fun = fun
  )
  class(out) <- c(
    "essandwich",
    class(out)
  )
  out
}
