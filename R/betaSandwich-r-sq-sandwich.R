#' Estimate R-Squared and Adjusted R-Squared and the Corresponding Sampling
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
#' The R-squared and the adjusted R-squared are
#' \deqn{R^{2} = \boldsymbol{\beta}^{\ast\prime} \boldsymbol{R}_{x}
#' \boldsymbol{\beta}^{\ast}, \quad \bar{R}^{2} = 1 - \left( 1 - R^{2} \right)
#' \frac{n - 1}{n - k},}
#' where \eqn{n} is the sample size and \eqn{k} is the number of parameters
#' in the regression model, including the intercept.
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
#'     \item{vcov}{Sampling covariance matrix of R-squared and adjusted
#'       R-squared.}
#'     \item{est}{Vector of R-squared and adjusted R-squared.}
#'     \item{fun}{Function used ("RSqSandwich").}
#'   }
#'
#' @param object Object of class `betasandwich`,
#'   that is,
#'   the output of the [BetaHC()], [BetaN()], or [BetaADF()] functions.
#' @param alpha Numeric vector.
#'   Significance level \eqn{\alpha}.
#'
#' @examples
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' std <- BetaHC(object)
#' out <- RSqSandwich(std)
#' # Methods -------------------------------------------------------
#' print(out)
#' summary(out)
#' coef(out)
#' vcov(out)
#' confint(out, level = 0.95)
#'
#' @family Beta Sandwich Functions
#' @keywords betaSandwich rsq
#' @export
RSqSandwich <- function(object,
                        alpha = c(0.05, 0.01, 0.001)) {
  .CheckSandwich(object = object)
  lm_process <- object$lm_process
  jcap <- .JacRSqWRTThetaStar(
    betastar = lm_process$betastar,
    rhocapx = lm_process$rhocap[
      2:lm_process$k,
      2:lm_process$k,
      drop = FALSE
    ]
  )
  .ESSandwich(
    object = object,
    jcap = rbind(
      jcap,
      ((lm_process$n - 1) / (lm_process$n - lm_process$k)) * jcap
    ),
    est = lm_process$rsq,
    alpha = alpha,
    fun = "RSqSandwich",
    call = match.call()
  )
}
