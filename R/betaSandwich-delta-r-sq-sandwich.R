#' Estimate Improvement in R-Squared and the Corresponding Sampling Covariance
#' Matrix
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @details The sampling covariance matrix is obtained by the delta method
#'   from the
#'   sampling covariance matrix of the standardized parameters stored in
#'   `object` (normal-theory, asymptotically distribution-free, or
#'   heteroskedasticity-consistent). Two or more regressors are required.
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
#' The improvement in R-squared for regressor \eqn{j} is
#' \deqn{\Delta R^{2}_{j} = R^{2} - R^{2}_{-j} = sr^{2}_{j}.}
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
#'     \item{vcov}{Sampling covariance matrix of improvement in R-squared
#'       (squared semipartial correlation coefficients).}
#'     \item{est}{Vector of improvement in R-squared (squared semipartial
#'       correlation coefficients).}
#'     \item{fun}{Function used ("DeltaRSqSandwich").}
#'   }
#'
#' @inheritParams RSqSandwich
#'
#' @references
#' Algina, J., Keselman, H. J., & Penfield, R. D. (2007).
#' Confidence intervals for an effect size measure
#' in multiple linear regression.
#' *Educational and Psychological Measurement*, *67*(2), 207--218.
#' \doi{10.1177/0013164406292030}
#'
#' Algina, J., Keselman, H. J., & Penfield, R. D. (2008a).
#' Confidence intervals for the squared multiple semipartial
#' correlation coefficient.
#' *Journal of Modern Applied Statistical Methods*, *7*(1), 2--10.
#' \doi{10.22237/jmasm/1209614460}
#'
#' Algina, J., Keselman, H. J., & Penfield, R. D. (2008b).
#' Note on a confidence interval for the squared semipartial
#' correlation coefficient.
#' *Educational and Psychological Measurement*, *68*(5), 734--741.
#' \doi{10.1177/0013164407313371}
#'
#' Algina, J., Keselman, H. J., & Penfield, R. D. (2010).
#' Confidence intervals for squared semipartial correlation coefficients:
#' The effect of nonnormality.
#' *Educational and Psychological Measurement*, *70*(6), 926--940.
#' \doi{10.1177/0013164410379335}
#'
#' @examples
#' object <- stats::lm(
#'   formula = QUALITY ~ NARTIC + PCTGRT + PCTSUPP,
#'   data = nas1982
#' )
#' std <- BetaHC(object = object)
#' out <- DeltaRSqSandwich(object = std)
#' # Methods -------------------------------------------------------
#' print(x = out)
#' summary(object = out)
#' coef(object = out)
#' vcov(object = out)
#' confint(object = out, level = 0.95)
#'
#' @family Beta Sandwich Functions
#' @keywords betaSandwich deltarsq
#' @export
DeltaRSqSandwich <- function(object,
                             alpha = c(0.05, 0.01, 0.001)) {
  .CheckSandwich(
    object = object,
    p_min = 2
  )
  lm_process <- object$lm_process
  .ESSandwich(
    object = object,
    jcap = .JacDeltaRSqWRTThetaStar(
      betastar = lm_process$betastar,
      rhocapx = lm_process$rhocap[
        2:lm_process$k,
        2:lm_process$k,
        drop = FALSE
      ]
    ),
    est = lm_process$scor^2,
    alpha = alpha,
    fun = "DeltaRSqSandwich",
    call = match.call()
  )
}
