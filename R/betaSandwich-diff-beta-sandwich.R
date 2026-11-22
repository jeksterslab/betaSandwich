#' Estimate Differences of Standardized Regression Coefficients
#' and the Corresponding Sampling Covariance Matrix
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @details
#' For every pair of regressors \eqn{j < l},
#' the difference of the standardized regression slopes is
#' \deqn{\Delta \beta^{\ast}_{jl} = \beta^{\ast}_{j} - \beta^{\ast}_{l}.}
#' Collect the differences as \eqn{\boldsymbol{C} \boldsymbol{\beta}^{\ast}},
#' where each row of \eqn{\boldsymbol{C}} has \eqn{1} in column \eqn{j},
#' \eqn{-1} in column \eqn{l}, and zeros elsewhere.
#' The sampling covariance matrix is
#' \deqn{\widehat{\operatorname{var}} \left( \boldsymbol{C}
#' \hat{\boldsymbol{\beta}}^{\ast} \right) = \boldsymbol{C}
#' \widehat{\operatorname{var}} \left( \hat{\boldsymbol{\beta}}^{\ast} \right)
#' \boldsymbol{C}^{\prime},}
#' with \eqn{\widehat{\operatorname{var}} \left(
#' \hat{\boldsymbol{\beta}}^{\ast} \right)} from [BetaHC()], [BetaN()], or
#' [BetaADF()],
#' and the confidence intervals are
#' \eqn{\Delta \hat{\beta}^{\ast}_{jl} \pm z_{1 - \alpha / 2}
#' \widehat{\operatorname{se}} \left( \Delta \hat{\beta}^{\ast}_{jl} \right)}.
#'
#' @return Returns an object of class `essandwich`
#'   (and `diffbetasandwich`)
#'   which is a list with the following elements:
#'   \describe{
#'     \item{call}{Function call.}
#'     \item{fit}{The argument `object`.}
#'     \item{args}{Function arguments.}
#'     \item{vcov}{Sampling covariance matrix of
#'       differences of standardized slopes.}
#'     \item{est}{Vector of
#'       differences of standardized slopes.}
#'     \item{fun}{Function used ("DiffBetaSandwich").}
#'   }
#'
#' @param object Object of class `betasandwich`,
#'   that is,
#'   the output of the [BetaHC()], [BetaN()], or [BetaADF()] functions.
#' @param alpha Numeric vector.
#'   Significance level \eqn{\alpha}.
#'
#' @examples
#' # Data ---------------------------------------------------------------------
#' data("nas1982", package = "betaSandwich")
#'
#' # Fit Model in lm ----------------------------------------------------------
#' object <- stats::lm(
#'   formula = QUALITY ~ NARTIC + PCTGRT + PCTSUPP,
#'   data = nas1982
#' )
#'
#' # BetaHC -------------------------------------------------------------------
#' std <- BetaHC(object = object)
#'
#' # DiffBetaSandwich ---------------------------------------------------------
#' diff <- DiffBetaSandwich(object = std)
#'
#' ## Methods -----------------------------------------------------------------
#' print(x = diff)
#' summary(object = diff)
#' coef(object = diff)
#' vcov(object = diff)
#' confint(object = diff, level = 0.95)
#'
#' @family Beta Sandwich Functions
#' @keywords betaSandwich diff
#' @export
DiffBetaSandwich <- function(object,
                             alpha = c(0.05, 0.01, 0.001)) {
  stopifnot(
    inherits(
      x = object,
      what = "betasandwich"
    )
  )
  if (object$lm_process$p < 2) {
    stop("Two or more regressors is required.")
  }
  if (isFALSE(x = attributes(x = object)$std)) {
    stop("Differences need fully standardized slopes.")
  }
  est <- object$lm_process$dif_betastar
  jcap <- .JacDiffBetaStar(
    p = object$lm_process$p
  )
  vcov <- object$vcov[
    seq_len(length.out = object$lm_process$p),
    seq_len(length.out = object$lm_process$p),
    drop = FALSE
  ]
  vcov <- jcap %*% tcrossprod(
    x = vcov,
    y = jcap
  )
  colnames(
    x = vcov
  ) <- rownames(x = vcov) <- names(x = object$lm_process$dif_betastar)
  out <- list(
    call = match.call(),
    fit = object,
    args = list(
      object = object,
      alpha = alpha
    ),
    vcov = vcov,
    est = est,
    fun = "DiffBetaSandwich"
  )
  class(x = out) <- c(
    "essandwich",
    "diffbetasandwich",
    class(x = out)
  )
  out
}
