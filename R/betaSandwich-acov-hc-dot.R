#' Asymptotic Covariance Matrix of the Standardized Parameter Vector
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param jcap Numeric matrix.
#'   Jacobian matrix of the half-vectorization
#'   of the model-implied covariance matrix
#'   with respect to the standardized parameter vector.
#' @param gammacap Numeric matrix.
#'   Adjusted asymptotic covariance matrix.
#' @param gammacap_mvn Numeric matrix.
#'   Asymptotic covariance matrix assuming multivariate normal distribution.
#'
#' @family BetaSandwich Functions
#' @keywords betaSandwich acov dot internal
.AcovHC <- function(jcap,
                    gammacap,
                    gammacap_mvn) {
  inversemvn <- solve(gammacap_mvn)
  tjcapinversemvn <- t(jcap) %*% inversemvn
  bread <- solve(tjcapinversemvn %*% jcap)
  meat <- tjcapinversemvn %*% gammacap %*% inversemvn %*% jcap
  bread %*% meat %*% bread
}
