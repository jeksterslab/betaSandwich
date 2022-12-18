# Process the lm object
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object Object of class `lm`.
#'
#' @family Beta Sandwich Functions
#' @keywords betaSandwich lm internal
#' @noRd
.ProcessLM <- function(object) {
  stopifnot(
    methods::is(
      object,
      "lm"
    )
  )
  y <- object$model[, 1]
  x <- stats::model.matrix(object)
  beta <- object$coefficients[-1]
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
  list(
    y = y,
    x = x,
    dims = dims,
    n = n,
    k = k,
    p = p,
    df = df,
    varnames = varnames,
    xnames = xnames,
    sigmacap = sigmacap,
    sigma = sigma,
    rhocap = rhocap,
    betastar = betastar,
    beta = beta
  )
}
