#' Wald Confidence Intervals
#'
#' Generates Wald Confidence Intervals
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a numeric matrix with the following variables:
#'   \describe{
#'     \item{est}{Estimates}
#'     \item{se}{Standard errors}
#'     \item{t or z}{Test statistics}
#'     \item{p}{p value}
#'     \item{ci}{Confidence intervals}
#'   }
#'   Note that if `test = TRUE`, the `ci` columns are omitted.
#'
#' @param est Numeric vector.
#'   Estimates.
#' @param se Numeric vector.
#'   Standard errors of estimates.
#' @param theta Numeric vector.
#'   Parameter values when the null hypothesis is true.
#' @param alpha Numeric vector.
#'   Significance level/s.
#' @param z Logical.
#'   If `z = TRUE`,
#'   use the standard normal distribution.
#'   If `z = FALSE`,
#'   use the t distribution.
#' @param df Numeric.
#'   Degrees of freedom if `z = FALSE`.
#' @param test Logical.
#'   If `TRUE`,
#'   return only the results of hypothesis tests.
#'   If `FALSE`,
#'   return both results of hypothesis tests and confidence intervals.
#'
#' @family Wald Confidence Intervals Functions
#' @keywords wald ci internal
#' @noRd
.CIWald <- function(est,
                    se,
                    theta = 0,
                    alpha = c(0.05, 0.01, 0.001),
                    z = FALSE,
                    df,
                    test = FALSE) {
  probs <- .ProbsofAlpha(alpha = alpha)
  stat <- (est - theta) / se
  if (z) {
    foo <- function(probs,
                    est,
                    se) {
      est + stats::qnorm(probs) * se
    }
    p <- 2 * stats::pnorm(-abs(stat))
    stat_name <- "z"
  } else {
    foo <- function(probs,
                    est,
                    se) {
      est + stats::qt(probs, df = df) * se
    }
    p <- 2 * stats::pt(-abs(stat), df = df)
    stat_name <- "t"
  }
  out <- cbind(
    est,
    se,
    stat,
    p
  )
  varnames <- c(
    "est",
    "se",
    stat_name,
    "p"
  )
  if (!test) {
    ci <- lapply(
      X = probs,
      FUN = foo,
      est = est,
      se = se
    )
    ci <- do.call(
      what = "cbind",
      args = ci
    )
    varnames <- c(
      varnames,
      paste0(probs * 100, "%")
    )
    out <- cbind(
      out,
      ci
    )
  }
  colnames(out) <- varnames
  return(
    out
  )
}
