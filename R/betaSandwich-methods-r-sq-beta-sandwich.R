#' Print Method for an Object of Class `rsqbetasandwich`
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Prints a matrix of
#'   multiple correlation coefficients
#'   (R-squared and adjusted R-squared),
#'   standard errors,
#'   test statistics,
#'   degrees of freedom,
#'   p-values,
#'   and
#'   confidence intervals.
#'
#' @param x Object of class `rsqbetasandwich`.
#' @param ... additional arguments.
#' @param alpha Numeric vector.
#'   Significance level \eqn{\alpha}.
#'   If `alpha = NULL`,
#'   use the argument `alpha` used in `x`.
#' @param digits Digits to print.
#'
#' @examples
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' std <- BetaHC(object)
#' rsq <- RSqBetaSandwich(std)
#' print(rsq)
#'
#' @keywords methods
#' @export
print.rsqbetasandwich <- function(x,
                                  alpha = NULL,
                                  digits = 4,
                                  ...) {
  print.summary.rsqbetasandwich(
    summary.rsqbetasandwich(
      object = x,
      alpha = alpha,
      digits = digits
    )
  )
}

#' Summary Method for an Object of Class `rsqbetasandwich`
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a matrix of
#'   multiple correlation coefficients
#'   (R-squared and adjusted R-squared),
#'   standard errors,
#'   test statistics,
#'   degrees of freedom,
#'   p-values,
#'   and
#'   confidence intervals.
#'
#' @param object Object of class `rsqbetasandwich`.
#' @param ... additional arguments.
#' @param alpha Numeric vector.
#'   Significance level \eqn{\alpha}.
#'   If `alpha = NULL`,
#'   use the argument `alpha` used in `object`.
#' @param digits Digits to print.
#'
#' @examples
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' std <- BetaHC(object)
#' rsq <- RSqBetaSandwich(std)
#' summary(rsq)
#'
#' @keywords methods
#' @export
summary.rsqbetasandwich <- function(object,
                                    alpha = NULL,
                                    digits = 4,
                                    ...) {
  ci <- .RSqCI(
    object = object,
    alpha = alpha
  )
  print_summary <- round(
    x = ci,
    digits = digits
  )
  attr(
    x = ci,
    which = "fit"
  ) <- object
  attr(
    x = ci,
    which = "print_summary"
  ) <- print_summary
  attr(
    x = ci,
    which = "alpha"
  ) <- alpha
  attr(
    x = ci,
    which = "digits"
  ) <- digits
  class(ci) <- "summary.rsqbetasandwich"
  ci
}

#' @noRd
#' @keywords internal
#' @exportS3Method print summary.rsqbetasandwich
print.summary.rsqbetasandwich <- function(x,
                                          ...) {
  print_summary <- attr(
    x = x,
    which = "print_summary"
  )
  object <- attr(
    x = x,
    which = "fit"
  )
  cat("Call:\n")
  base::print(object$call)
  cat(
    "\nMultiple correlation with",
    toupper(object$fit$args$type),
    "standard errors:\n"
  )
  print(print_summary)
  invisible(x)
}

#' Sampling Covariance Matrix of
#' Multiple Correlation Coefficients
#' (R-Squared and Adjusted R-Squared)
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a matrix of the
#'   variance-covariance matrix
#'   of multiple correlation coefficients
#'   (R-squared and adjusted R-squared).
#'
#' @param object Object of class `rsqbetasandwich`.
#' @param ... additional arguments.
#'
#' @examples
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' std <- BetaHC(object)
#' rsq <- RSqBetaSandwich(std)
#' vcov(rsq)
#'
#' @keywords methods
#' @export
vcov.rsqbetasandwich <- function(object,
                                 ...) {
  .RSqCov(object)
}

#' Multiple Correlation Coefficients
#' (R-Squared and Adjusted R-Squared)
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a vector of multiple correlation coefficients
#' (R-squared and adjusted R-squared)
#'
#' @param object Object of class `rsqbetasandwich`.
#' @param ... additional arguments.
#'
#' @examples
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' std <- BetaHC(object)
#' rsq <- RSqBetaSandwich(std)
#' coef(rsq)
#'
#' @keywords methods
#' @export
coef.rsqbetasandwich <- function(object,
                                 ...) {
  c(
    rsq = object$fit$lm_process$rsq[1],
    adj = object$fit$lm_process$rsq[2]
  )
}

#' Confidence Intervals for
#' Multiple Correlation Coefficients
#' (R-Squared and Adjusted R-Squared)
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a matrix of confidence intervals.
#'
#' @param object Object of class `rsqbetasandwich`.
#' @param ... additional arguments.
#' @param parm a specification of which parameters
#'   are to be given confidence intervals,
#'   either a vector of numbers or a vector of names.
#'   If missing, all parameters are considered.
#' @param level the confidence level required.
#'
#' @examples
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' std <- BetaHC(object)
#' rsq <- RSqBetaSandwich(std)
#' confint(rsq, level = 0.95)
#'
#' @keywords methods
#' @export
confint.rsqbetasandwich <- function(object,
                                    parm = NULL,
                                    level = 0.95,
                                    ...) {
  if (is.null(parm)) {
    parm <- seq_len(2)
  }
  ci <- .RSqCI(
    object = object,
    alpha = 1 - level[1]
  )[parm, 6:7, drop = FALSE]
  varnames <- colnames(ci)
  varnames <- gsub(
    pattern = "%",
    replacement = " %",
    x = varnames
  )
  colnames(ci) <- varnames
  ci
}
