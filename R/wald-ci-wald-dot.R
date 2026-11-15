#' Wald Tests and Confidence Intervals (Internal)
#'
#' Computes, for each parameter,
#' the Wald test statistic
#' \eqn{\left( \hat{\theta} - \theta_{0} \right) / \mathrm{se}},
#' its two-sided \eqn{p} value,
#' and Wald confidence intervals,
#' from the standard normal or the \eqn{t} distribution.
#' No input checks: see `CIWald()`.
#'
#' @author Ivan Jacob Agaloos Pesigan
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
#' @return Returns a numeric matrix with one row per parameter
#'   and the following columns:
#'   \describe{
#'     \item{est}{Estimates.}
#'     \item{se}{Standard errors.}
#'     \item{z or t}{Test statistics.}
#'     \item{df}{Degrees of freedom (`z = FALSE` only).}
#'     \item{p}{Two-sided p values.}
#'     \item{ci}{Confidence limits, one column per probability,
#'       named as percentages, for example `2.5%` and `97.5%`.}
#'   }
#'   If `test = TRUE`, the confidence limits are omitted.
#'
#' @examples
#' .CIWald(
#'   est = c(0.5, 0.25),
#'   se = c(0.1, 0.1),
#'   theta = 0,
#'   alpha = 0.05,
#'   z = TRUE
#' )
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
      est + stats::qnorm(p = probs) * se
    }
    p <- 2 * stats::pnorm(q = -abs(x = stat))
    out <- cbind(
      est,
      se,
      stat,
      p
    )
    varnames <- c(
      "est",
      "se",
      "z",
      "p"
    )
  } else {
    foo <- function(probs,
                    est,
                    se) {
      est + stats::qt(p = probs, df = df) * se
    }
    p <- 2 * stats::pt(q = -abs(x = stat), df = df)
    out <- cbind(
      est,
      se,
      stat,
      df,
      p
    )
    varnames <- c(
      "est",
      "se",
      "t",
      "df",
      "p"
    )
  }
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
  colnames(x = out) <- varnames
  out
}
