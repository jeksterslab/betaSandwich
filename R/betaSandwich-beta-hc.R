#' Estimate Standardized Regression Coefficients and Robust Sampling Covariance Matrix
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
#' @param type Character string.
#'   Correction type.
#'   Possible values are
#'   `"hc0"`,
#'   `"hc1"`,
#'   `"hc2"`,
#'   `"hc3"`,
#'   `"hc4"`,
#'   `"hc4m"`, and
#'   `"hc5"`.
#' @param g1 Numeric.
#'   `g1` value for `type = "hc4m"` or `type = "hc5"`.
#' @param g2 Numeric.
#'   `g2` value for `type = "hc4m"`.
#' @param k Numeric.
#'   Constant for `type = "hc5"`
#' @references
#' Dudgeon, P. (2017).
#' Some improvements in confidence intervals for standardized regression coefficients.
#' *Psychometrika*, *82*(4), 928–951.
#' \doi{10.1007/s11336-017-9563-z}
#' @examples
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' std <- BetaHC(object)
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
BetaHC <- function(object,
                   type = "hc3",
                   g1 = 1,
                   g2 = 1.5,
                   k = 0.7) {
  stopifnot(
    methods::is(
      object,
      "lm"
    )
  )
  stopifnot(
    type %in% c(
      "hc0",
      "hc1",
      "hc2",
      "hc3",
      "hc4",
      "hc4m",
      "hc5"
    )
  )
  stopifnot(0 < k & k < 1)
  constant <- k
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
  gammacap <- .GammaHC(
    d = .DofMat(
      x,
      center = colMeans(x),
      n = n,
      k = k
    ),
    sigmacap = sigmacap,
    qcap = .QMat(
      h = stats::hatvalues(object),
      k = k,
      type = type,
      g1 = g1,
      g2 = g2,
      constant = constant
    ),
    n = n
  )
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
  avcov <- .ACovHC(
    jcap = jcap,
    gammacap = gammacap,
    gammacap_mvn = gammacap_mvn
  )
  vcov <- .CovHC(
    acov = avcov,
    type = type,
    n = n,
    df = df
  )[1:p, 1:p, drop = FALSE]
  colnames(vcov) <- rownames(vcov) <- xnames
  out <- list(
    call = match.call(),
    type = type,
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
