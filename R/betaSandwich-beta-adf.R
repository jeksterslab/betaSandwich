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
  input <- .ProcessLM(object)
  jcap <- .JacobianVechSigmaWRTThetaStar(
    betastar = input$betastar,
    sigmay = input$sigma[1],
    sigmax = input$sigma[-1],
    rhocapx = input$rhocap[2:input$k, 2:input$k, drop = FALSE],
    q = input$p + 1 + 0.5 * input$p * (input$p + 1),
    p = input$p
  )
  sigmacap_consistent <- (
    input$sigmacap * (
      input$n - 1
    ) / input$n
  )
  vechsigmacap_consistent <- .Vech(
    sigmacap_consistent
  )
  gammacap_adf <- .GammaADFUnbiased(
    gammacapadf_consistent = .GammaADFConsistent(
      d = .DofMat(
        input$x,
        center = colMeans(input$x),
        n = input$n,
        k = input$k
      ),
      vechsigmacap_consistent = vechsigmacap_consistent,
      n = input$n
    ),
    gammacapmvn_consistent = .GammaN(
      sigmacap = sigmacap_consistent,
      pinv_of_dcap = .PInvDmat(.DMat(input$k))
    ),
    vechsigmacap_consistent = vechsigmacap_consistent,
    n = input$n
  )
  # the procedure from here on is the same as normal
  avcov <- .ACovN(
    jcap = jcap,
    gammacap_mvn = gammacap_adf
  )
  vcov <- .CovN(
    acov = avcov,
    n = input$n
  )[1:input$p, 1:input$p, drop = FALSE]
  colnames(vcov) <- rownames(vcov) <- input$xnames
  out <- list(
    call = match.call(),
    type = "adf",
    beta = input$betastar,
    vcov = vcov,
    n = input$n,
    p = input$p,
    df = input$df
  )
  class(out) <- c(
    "betasandwich",
    class(out)
  )
  return(
    out
  )
}
