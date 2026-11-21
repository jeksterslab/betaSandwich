#' Estimate Cohen's f-Squared and Local f-Squared and the Corresponding
#' Sampling
#' Covariance Matrix
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @details The sampling covariance matrix is obtained by the delta method
#'   from the
#'   sampling covariance matrix of the standardized parameters stored in
#'   `object` (normal-theory, asymptotically distribution-free, or
#'   heteroskedasticity-consistent).
#'
#' @details
#' Let \eqn{\boldsymbol{\beta}^{\ast}} be the vector of standardized slopes,
#' \eqn{\boldsymbol{R}_{x}} the correlation matrix of the regressors,
#' \eqn{r^{jj}} the \eqn{j}th diagonal element of \eqn{\boldsymbol{R}_{x}^{-1}},
#' and \eqn{R^{2} = \boldsymbol{\beta}^{\ast\prime} \boldsymbol{R}_{x}
#' \boldsymbol{\beta}^{\ast}}.
#' The semipartial correlation of regressor \eqn{j} is
#' \deqn{sr_{j} = \beta^{\ast}_{j} / \sqrt{r^{jj}} = \beta^{\ast}_{j} \sqrt{1 -
#' R^{2}_{j \cdot -j}},}
#' where \eqn{R^{2}_{j \cdot -j}} is the R-squared from regressing regressor
#' \eqn{j}
#' on the other regressors.
#' Its square, \eqn{sr^{2}_{j}}, is the change in \eqn{R^{2}} when regressor
#' \eqn{j} is added last.
#' Because \eqn{1 / r^{jj} = 1 - R^{2}_{j \cdot -j}}, \eqn{sr_{j}} shrinks
#' toward zero
#' as regressor \eqn{j} becomes more redundant with the other regressors.
#' Cohen's f-squared and the local f-squared of regressor \eqn{j} are
#' \deqn{f^{2} = \frac{R^{2}}{1 - R^{2}}, \quad f^{2}_{j} = \frac{sr^{2}_{j}}{1
#' - R^{2}}.}
#' By convention (Cohen, 1988), values of about 0.02, 0.15, and 0.35 are small,
#' medium, and large effects.
#' For an effect size \eqn{g\left( \boldsymbol{\theta}^{\ast} \right)}
#' that is a function of the standardized parameters,
#' the sampling variance is approximated by the delta method,
#' \deqn{\widehat{\operatorname{var}} \left( \hat{g} \right) =
#' \boldsymbol{J}_{g} \,
#' \widehat{\boldsymbol{\Sigma}}_{\boldsymbol{\theta}^{\ast}} \,
#' \boldsymbol{J}_{g}^{\prime},}
#' where \eqn{\boldsymbol{J}_{g}} is the Jacobian of \eqn{g} and
#' \eqn{\widehat{\boldsymbol{\Sigma}}_{\boldsymbol{\theta}^{\ast}}} is the
#' sampling covariance matrix
#' from [BetaHC()], [BetaADF()], or [BetaN()].
#'
#' @return Returns an object of class `essandwich`
#'   which is a list with the following elements:
#'   \describe{
#'     \item{call}{Function call.}
#'     \item{fit}{The argument `object`.}
#'     \item{args}{Function arguments.}
#'     \item{vcov}{Sampling covariance matrix of Cohen's f-squared (fsq) and
#'       the local f-squared of each regressor.}
#'     \item{est}{Vector of Cohen's f-squared (fsq) and the local f-squared of
#'       each regressor.}
#'     \item{fun}{Function used ("FSqSandwich").}
#'   }
#'
#' @inheritParams RSqSandwich
#'
#' @references
#' Selya, A. S., Rose, J. S., Dierker, L. C., Hedeker, D., & Mermelstein,
#' R. J. (2012).
#' A practical guide to calculating Cohen's f2,
#' a measure of local effect size, from PROC MIXED.
#' *Frontiers in Psychology*, *3*, 111.
#' \doi{10.3389/fpsyg.2012.00111}
#'
#' @examples
#' object <- stats::lm(
#'   formula = QUALITY ~ NARTIC + PCTGRT + PCTSUPP,
#'   data = nas1982
#' )
#' std <- BetaHC(object = object)
#' out <- FSqSandwich(object = std)
#' # Methods -------------------------------------------------------
#' print(x = out)
#' summary(object = out)
#' coef(object = out)
#' vcov(object = out)
#' confint(object = out, level = 0.95)
#'
#' @family Beta Sandwich Functions
#' @keywords betaSandwich fsq
#' @export
FSqSandwich <- function(object,
                        alpha = c(0.05, 0.01, 0.001)) {
  .CheckSandwich(object = object)
  lm_process <- object$lm_process
  rhocapx <- lm_process$rhocap[
    2:lm_process$k,
    2:lm_process$k,
    drop = FALSE
  ]
  rsq <- lm_process$rsq[1]
  srsq <- lm_process$betastar^2 / diag(
    x = chol2inv(
      x = chol(x = rhocapx)
    )
  )
  est <- c(
    rsq / (1 - rsq),
    srsq / (1 - rsq)
  )
  names(x = est) <- c(
    "fsq",
    lm_process$xnames
  )
  .ESSandwich(
    object = object,
    jcap = .JacFSqWRTThetaStar(
      betastar = lm_process$betastar,
      rhocapx = rhocapx
    ),
    est = est,
    alpha = alpha,
    fun = "FSqSandwich",
    call = match.call()
  )
}
