#' Estimate Standardized Regression Coefficients and Sampling Covariance Matrix Assuming Multivariate Normality
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns an object of class `betaSandwich` which is a list with the following elements:
#' \describe{
#'   \item{call}{Function call.}
#'   \item{type}{Standard error type.}
#'   \item{beta}{Vector of standardized slopes.}
#'   \item{vcov}{Sampling covariance matrix of the standardized slopes.}
#'   \item{n}{Sample size.}
#'   \item{p}{Number of regressors.}
#'   \item{df}{\eqn{n - p - 1} degrees of freedom}
#' }
#' @param object Object of class `lm`.
#' @examples
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' std <- BetaN(object)
#' # Methods -------------------------------------------------------
#' print(std)
#' summary(std)
#' coef(std)
#' vcov(std)
#' confint(std, level = 0.95)
#' @export
#' @importFrom methods is
#' @importFrom stats cov hatvalues
#' @family Beta Sandwich Functions
#' @keywords betaSandwich
BetaN <- function(object) {
  stopifnot(
    methods::is(
      object,
      "lm"
    )
  )
  y <- object$model[, 1]
  x <- stats::model.matrix(object)
  x[, 1] <- y
  varnames <- colnames(x)
  xnames <- varnames[-1]
  dims <- dim(x)
  n <- dims[1]
  k <- dims[2]
  p <- k - 1
  df <- n - k
  sigmacap <- stats::cov(x)
  sigma <- sqrt(diag(sigmacap))
  rhocap <- .RhoofSigma(
    sigmacap,
    q = 1 / sigma
  )
  betastar <- .BetaStarofRho(
    rhocap = rhocap,
    k = k
  )
  names(betastar) <- xnames
  jcap <- .JacobianVechSigmaWRTThetaStar(
    betastar = betastar,
    sigmay = sigma[1],
    sigmax = sigma[-1],
    rhocapx = rhocap[2:k, 2:k, drop = FALSE],
    q = p + 1 + 0.5 * p * (p + 1),
    p = p
  )
  gammacap_mvn <- .GammaN(
    sigmacap = sigmacap,
    pinv_of_dcap = .PInvDmat(.DMat(k))
  )
  avcov <- .ACovN(
    jcap = jcap,
    gammacap_mvn = gammacap_mvn
  )
  vcov <- .CovN(
    acov = avcov,
    n = n
  )[1:p, 1:p, drop = FALSE]
  colnames(vcov) <- rownames(vcov) <- xnames
  out <- list(
    call = match.call(),
    type = "mvn",
    beta = betastar,
    vcov = vcov,
    n = n,
    p = p,
    df = df
  )
  class(out) <- c(
    "betasandwich",
    class(out)
  )
  return(
    out
  )
}
