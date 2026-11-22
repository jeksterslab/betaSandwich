#' Estimate Standardized Regression Coefficients
#' and the Corresponding Sampling Covariance Matrix
#' Using the Asymptotic Distribution-Free Approach
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @details
#' Note that while the calculation in [BetaADF()]
#' is different from `betaDelta::BetaDelta()` with `type = "adf"`,
#' the results are numerically equivalent.
#' [BetaADF()] is appropriate when sample sizes are moderate to large
#' (`n > 250`).
#' [BetaHC()] is recommended in most situations.
#'
#' @details
#' Partition the covariance matrix of the outcome and the regressors as
#' \deqn{\boldsymbol{\Sigma} = \begin{pmatrix} \sigma^{2}_{y} &
#' \boldsymbol{\sigma}_{yx}^{\prime} \\ \boldsymbol{\sigma}_{yx} &
#' \boldsymbol{\Sigma}_{x} \end{pmatrix}.}
#' The regression slopes and the standardized regression slopes are
#' \deqn{\boldsymbol{\beta} = \boldsymbol{\Sigma}_{x}^{-1}
#' \boldsymbol{\sigma}_{yx}, \quad \beta^{\ast}_{j} = \beta_{j}
#' \frac{\sigma_{x_{j}}}{\sigma_{y}},}
#' where \eqn{\sigma_{x_{j}}} is the standard deviation of regressor \eqn{j}.
#' With `std_y = FALSE`, \eqn{\sigma_{y}} is replaced by one;
#' for a regressor left out of `std_x`, \eqn{\sigma_{x_{j}}} is replaced by one.
#'
#' Let \eqn{\boldsymbol{\theta}^{\ast}} collect the standardized slopes,
#' \eqn{\sigma_{y}}, the standard deviations of the regressors,
#' and the correlations of the regressors,
#' and let \eqn{\boldsymbol{J} = \partial \operatorname{vech} \left(
#' \boldsymbol{\Sigma} \right) / \partial \boldsymbol{\theta}^{\ast \prime}}
#' be the Jacobian of the implied covariance matrix,
#' evaluated at the estimates (Dudgeon, 2017).
#' The asymptotically distribution-free sampling covariance matrix is
#' \deqn{\widehat{\operatorname{var}} \left( \hat{\boldsymbol{\theta}}^{\ast}
#' \right) = \frac{1}{n} \left( \boldsymbol{J}^{\prime}
#' \hat{\boldsymbol{\Gamma}}^{-1} \boldsymbol{J} \right)^{-1},}
#' where \eqn{\hat{\boldsymbol{\Gamma}}} is the unbiased estimator
#' \deqn{\hat{\boldsymbol{\Gamma}} = \frac{n \left( n - 1 \right)}{\left( n - 2
#' \right) \left( n - 3 \right)} \hat{\boldsymbol{\Gamma}}_{\mathrm{ADF}} -
#' \frac{n}{\left( n - 2 \right) \left( n - 3 \right)} \left(
#' \hat{\boldsymbol{\Gamma}}_{\mathrm{N}} - \frac{2}{n - 1} \operatorname{vech}
#' \left( \tilde{\boldsymbol{S}} \right) \operatorname{vech} \left(
#' \tilde{\boldsymbol{S}} \right)^{\prime} \right),}
#' \eqn{\tilde{\boldsymbol{S}}} is the covariance matrix with divisor \eqn{n},
#' \eqn{\hat{\boldsymbol{\Gamma}}_{\mathrm{N}}} is the normal-theory matrix
#' (see [BetaN()]) at \eqn{\tilde{\boldsymbol{S}}}, and
#' \deqn{\hat{\boldsymbol{\Gamma}}_{\mathrm{ADF}} = \frac{1}{n}
#' \sum_{i = 1}^{n} \left[ \operatorname{vech} \left( \boldsymbol{d}_{i}
#' \boldsymbol{d}_{i}^{\prime} \right) - \operatorname{vech} \left(
#' \tilde{\boldsymbol{S}} \right) \right] \left[ \operatorname{vech} \left(
#' \boldsymbol{d}_{i} \boldsymbol{d}_{i}^{\prime} \right) - \operatorname{vech}
#' \left( \tilde{\boldsymbol{S}} \right) \right]^{\prime},}
#' with \eqn{\boldsymbol{d}_{i}} the deviation of observation \eqn{i} from the
#' sample means.
#' The block of the standardized slopes is returned.
#' The confidence intervals are
#' \eqn{\hat{\beta}^{\ast}_{j} \pm t_{1 - \alpha / 2, n - k}
#' \widehat{\operatorname{se}} \left( \hat{\beta}^{\ast}_{j} \right)},
#' where \eqn{k} is the number of regression parameters,
#' including the intercept.
#'
#' @return Returns an object
#'   of class `betasandwich` which is a list with the following elements:
#'   \describe{
#'     \item{call}{Function call.}
#'     \item{args}{Function arguments.}
#'     \item{lm_process}{Processed `lm` object.}
#'     \item{gamma_n}{Asymptotic covariance matrix
#'       of the sample covariance matrix
#'       assuming multivariate normality.}
#'     \item{gamma_hc}{Asymptotic covariance matrix
#'       HC correction.}
#'     \item{gamma}{Asymptotic covariance matrix
#'       of the sample covariance matrix.}
#'     \item{acov}{Asymptotic covariance matrix
#'       of the standardized slopes.}
#'     \item{vcov}{Sampling covariance matrix
#'       of the standardized slopes.}
#'     \item{est}{Vector of standardized slopes.}
#'   }
#'
#' @param object Object of class `lm`.
#' @param std_y Logical.
#'   Standardize \eqn{y}.
#'   With `std_y = FALSE`, \eqn{y} is left in its original units
#'   (\eqn{b_j \sigma_{x_j}}).
#' @param std_x Character vector or `NULL`.
#'   Names of the regressors to standardize.
#'   `NULL` (default) standardizes all regressors;
#'   `character(0)` standardizes none (\eqn{b_j / \sigma_y}).
#'   Leaving a dummy variable out keeps its 0-1 contrast.
#'   The delta method accounts for the sampling variability
#'   of the standard deviations.
#' @param alpha Numeric vector.
#'   Significance level \eqn{\alpha}.
#'
#' @references
#' Browne, M. W. (1984).
#' Asymptotically distribution-free methods
#' for the analysis of covariance structures.
#' *British Journal of Mathematical and Statistical Psychology*,
#' *37*(1), 62--83.
#' \doi{10.1111/j.2044-8317.1984.tb00789.x}
#'
#' Dudgeon, P. (2017).
#' Some improvements in confidence intervals
#' for standardized regression coefficients.
#' *Psychometrika*, *82*(4), 928--951.
#' \doi{10.1007/s11336-017-9563-z}
#'
#' Pesigan, I. J. A., Sun, R. W., & Cheung, S. F. (2023).
#' betaDelta and betaSandwich:
#' Confidence intervals for standardized regression coefficients in R.
#' *Multivariate Behavioral Research*.
#' \doi{10.1080/00273171.2023.2201277}
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
#' # BetaADF ------------------------------------------------------------------
#' std <- BetaADF(object = object)
#'
#' ## Methods -----------------------------------------------------------------
#' print(x = std)
#' summary(object = std)
#' coef(object = std)
#' vcov(object = std)
#' confint(object = std, level = 0.95)
#'
#' @family Beta Sandwich Functions
#' @keywords betaSandwich std
#' @export
BetaADF <- function(object,
                    alpha = c(0.05, 0.01, 0.001),
                    std_y = TRUE,
                    std_x = NULL) {
  stopifnot(
    inherits(
      x = object,
      what = "lm"
    )
  )
  lm_process <- .ProcessLM(object = object)
  index <- .BetaStarSelectIndex(
    std_x = std_x,
    xnames = lm_process$xnames
  )
  jcap <- .JacVechSigmaWRTThetaStar(
    betastar = lm_process$betastar,
    sigmay = lm_process$sigma[1],
    sigmax = lm_process$sigma[-1],
    rhocapx = lm_process$rhocap[
      2:lm_process$k,
      2:lm_process$k,
      drop = FALSE
    ],
    q = lm_process$q,
    p = lm_process$p
  )
  sigmacap_consistent <- (
    lm_process$sigmacap * (
      lm_process$n - 1
    ) / lm_process$n
  )
  vechsigmacap_consistent <- .Vech(
    x = sigmacap_consistent
  )
  gammacap_adf <- .GammaADFUnbiased(
    gammacapadf_consistent = .GammaADFConsistent(
      d = .DofMat(
        x = lm_process$x,
        center = colMeans(x = lm_process$x),
        n = lm_process$n,
        k = lm_process$k
      ),
      vechsigmacap_consistent = vechsigmacap_consistent,
      n = lm_process$n
    ),
    gammacapmvn_consistent = .GammaN(
      sigmacap = sigmacap_consistent,
      pinv_of_dcap = .PInvDmat(d = .DMat(k = lm_process$k))
    ),
    vechsigmacap_consistent = vechsigmacap_consistent,
    n = lm_process$n
  )
  # the procedure from here on is the same as normal
  acov <- chol2inv(
    x = chol(
      x = .ACovSEMInverse(
        jcap = jcap,
        acov = gammacap_adf
      )
    )
  )
  vcov <- (1 / lm_process$n) * acov
  vcov <- .BetaStarSelectVCov(
    vcov = vcov,
    lm_process = lm_process,
    std_y = std_y,
    std_x = index
  )
  colnames(x = vcov) <- rownames(x = vcov) <- lm_process$xnames
  out <- list(
    call = match.call(),
    args = list(
      object = object,
      type = "mvn",
      alpha = alpha,
      std_y = std_y,
      std_x = std_x
    ),
    lm_process = lm_process,
    gamma_n = .GammaN(
      sigmacap = lm_process$sigmacap,
      pinv_of_dcap = .PInvDmat(d = .DMat(k = lm_process$k))
    ),
    gamma_hc = NULL,
    gamma = gammacap_adf,
    acov = acov,
    vcov = vcov,
    est = .BetaStarSelectLM(
      lm_process = lm_process,
      std_y = std_y,
      std_x = index
    )
  )
  attributes(x = out)$std <- std_y && all(index)
  class(x = out) <- c(
    "betasandwich",
    class(x = out)
  )
  out
}
