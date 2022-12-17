#' Estimate Standardized Regression Coefficients
#' and Robust Sampling Covariance Matrix
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
#' Some improvements in confidence intervals
#' for standardized regression coefficients.
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
#' @family Beta Sandwich Functions
#' @keywords betaSandwich
BetaHC <- function(object,
                   type = "hc3",
                   g1 = 1,
                   g2 = 1.5,
                   k = 0.7) {
  input <- .ProcessLM(object)
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
  gammacap <- .GammaHC(
    d = .DofMat(
      input$x,
      center = colMeans(input$x),
      n = input$n,
      k = input$k
    ),
    sigmacap = input$sigmacap,
    qcap = .QMat(
      h = stats::hatvalues(object),
      k = input$k,
      type = type,
      g1 = g1,
      g2 = g2,
      constant = constant
    ),
    n = input$n
  )
  jcap <- .JacobianVechSigmaWRTThetaStar(
    betastar = input$betastar,
    sigmay = input$sigma[1],
    sigmax = input$sigma[-1],
    rhocapx = input$rhocap[2:input$k, 2:input$k, drop = FALSE],
    q = input$p + 1 + 0.5 * input$p * (input$p + 1),
    p = input$p
  )
  gammacap_mvn <- .GammaN(
    sigmacap = input$sigmacap,
    pinv_of_dcap = .PInvDmat(.DMat(input$k))
  )
  avcov <- .ACovHC(
    jcap = jcap,
    gammacap = gammacap,
    gammacap_mvn = gammacap_mvn
  )
  vcov <- .CovHC(
    acov = avcov,
    type = type,
    n = input$n,
    df = input$df
  )[1:input$p, 1:input$p, drop = FALSE]
  colnames(vcov) <- rownames(vcov) <- input$xnames
  out <- list(
    call = match.call(),
    type = type,
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
