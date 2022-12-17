#' Estimate Standardized Regression Coefficients
#' and Sampling Covariance Matrix
#' Using the Asymptotic Distribution-Free Approach
#'
#' @details
#' `BetaADF()` is appropriate when sample sizes are moderate to large
#' (`n > 250`).
#' `BetaHC()` is recommended in most situations.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns an object of class `betaSandwich`
#' which is a list with the following elements:
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
#' std <- BetaADF(object)
#' # Methods -------------------------------------------------------
#' print(std)
#' summary(std)
#' coef(std)
#' vcov(std)
#' confint(std, level = 0.95)
#' @export
#' @family Beta Sandwich Functions
#' @keywords betaSandwich
BetaADF <- function(object) {
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
  sigmacap_consistent <- (
    sigmacap * (
      n - 1
    ) / n
  )
  vechsigmacap_consistent <- .Vech(
    sigmacap_consistent
  )
  gammacap_adf <- .GammaADFUnbiased(
    gammacapadf_consistent = .GammaADFConsistent(
      d = .DofMat(
        x,
        center = colMeans(x),
        n = n,
        k = k
      ),
      vechsigmacap_consistent = vechsigmacap_consistent,
      n = n
    ),
    gammacapmvn_consistent = .GammaN(
      sigmacap = sigmacap_consistent,
      pinv_of_dcap = .PInvDmat(.DMat(k))
    ),
    vechsigmacap_consistent = vechsigmacap_consistent,
    n = n
  )
  # the procedure from here on is the same as normal
  avcov <- .ACovN(
    jcap = jcap,
    gammacap_mvn = gammacap_adf
  )
  vcov <- .CovN(
    acov = avcov,
    n = n
  )[1:p, 1:p, drop = FALSE]
  colnames(vcov) <- rownames(vcov) <- xnames
  out <- list(
    call = match.call(),
    type = "adf",
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
