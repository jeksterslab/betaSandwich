#' Print Method for an Object of Class `betasandwich`
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Prints a matrix of
#'   standardized regression slopes,
#'   standard errors,
#'   test statistics,
#'   degrees of freedom,
#'   p-values,
#'   and
#'   confidence intervals.
#'
#' @param x Object of class `betasandwich`.
#' @param ... additional arguments.
#' @param alpha Numeric vector.
#'   Significance level \eqn{\alpha}.
#'   If `alpha = NULL`,
#'   use the argument `alpha` used in `x`.
#' @param digits Digits to print.
#'
#' @examples
#' # Data ---------------------------------------------------------------------
#' data("nas1982", package = "betaSandwich")
#'
#' # Fit Model in lm ----------------------------------------------------------
#' object <- stats::lm(
#'   formula = QUALITY ~ NARTIC + PCTGRT + PCTSUPP,
#'   data = nas1982
#' )
#'
#' # BetaHC -------------------------------------------------------------------
#' std <- BetaHC(object = object)
#'
#' ## Methods -----------------------------------------------------------------
#' print(x = std)
#'
#' @keywords methods
#' @export
print.betasandwich <- function(x,
                               alpha = NULL,
                               digits = 4,
                               ...) {
  print.summary.betasandwich(
    x = summary.betasandwich(
      object = x,
      alpha = alpha,
      digits = digits
    )
  )
}

#' Summary Method for an Object of Class `betasandwich`
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @details
#' The confidence intervals are
#' \eqn{\hat{\theta} \pm t_{1 - \alpha / 2, n - k}
#' \widehat{\operatorname{se}} \left( \hat{\theta} \right)},
#' where \eqn{\widehat{\operatorname{se}}} is the square root of the
#' diagonal of the sampling covariance matrix
#' (see [BetaHC()], [BetaN()], and [BetaADF()]),
#' \eqn{n} is the sample size, and \eqn{k} is the number of regression
#' parameters, including the intercept.
#' The test statistic is
#' \eqn{\hat{\theta} / \widehat{\operatorname{se}} \left( \hat{\theta} \right)}
#' with \eqn{n - k} degrees of freedom.
#'
#' @return Returns a matrix of
#'   standardized regression slopes,
#'   standard errors,
#'   test statistics,
#'   degrees of freedom,
#'   p-values,
#'   and
#'   confidence intervals.
#'
#' @param object Object of class `betasandwich`.
#' @param ... additional arguments.
#' @param alpha Numeric vector.
#'   Significance level \eqn{\alpha}.
#'   If `alpha = NULL`,
#'   use the argument `alpha` used in `object`.
#' @param digits Digits to print.
#'
#' @examples
#' # Data ---------------------------------------------------------------------
#' data("nas1982", package = "betaSandwich")
#'
#' # Fit Model in lm ----------------------------------------------------------
#' object <- stats::lm(
#'   formula = QUALITY ~ NARTIC + PCTGRT + PCTSUPP,
#'   data = nas1982
#' )
#'
#' # BetaHC -------------------------------------------------------------------
#' std <- BetaHC(object = object)
#'
#' ## Methods -----------------------------------------------------------------
#' summary(object = std)
#'
#' @keywords methods
#' @export
summary.betasandwich <- function(object,
                                 alpha = NULL,
                                 digits = 4,
                                 ...) {
  ci <- .BetaCI(
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
  class(x = ci) <- "summary.betasandwich"
  ci
}

#' @noRd
#' @keywords internal
#' @exportS3Method print summary.betasandwich
print.summary.betasandwich <- function(x,
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
  base::print(x = object$call)
  cat(
    "\nStandardized regression slopes with",
    toupper(x = object$args$type),
    "standard errors:\n"
  )
  print(x = print_summary)
  invisible(x = x)
}

#' Sampling Covariance Matrix of the Standardized Regression Slopes
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a matrix of the
#'   variance-covariance matrix
#'   of standardized slopes.
#'
#' @param object Object of class `betasandwich`.
#' @param ... additional arguments.
#'
#' @examples
#' # Data ---------------------------------------------------------------------
#' data("nas1982", package = "betaSandwich")
#'
#' # Fit Model in lm ----------------------------------------------------------
#' object <- stats::lm(
#'   formula = QUALITY ~ NARTIC + PCTGRT + PCTSUPP,
#'   data = nas1982
#' )
#'
#' # BetaHC -------------------------------------------------------------------
#' std <- BetaHC(object = object)
#'
#' ## Methods -----------------------------------------------------------------
#' vcov(object = std)
#'
#' @keywords methods
#' @export
vcov.betasandwich <- function(object,
                              ...) {
  object$vcov[
    seq_len(length.out = object$lm_process$p),
    seq_len(length.out = object$lm_process$p),
    drop = FALSE
  ]
}

#' Standardized Regression Slopes
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a vector of standardized regression slopes.
#'
#' @param object Object of class `betasandwich`.
#' @param ... additional arguments.
#'
#' @examples
#' # Data ---------------------------------------------------------------------
#' data("nas1982", package = "betaSandwich")
#'
#' # Fit Model in lm ----------------------------------------------------------
#' object <- stats::lm(
#'   formula = QUALITY ~ NARTIC + PCTGRT + PCTSUPP,
#'   data = nas1982
#' )
#'
#' # BetaHC -------------------------------------------------------------------
#' std <- BetaHC(object = object)
#'
#' ## Methods -----------------------------------------------------------------
#' coef(object = std)
#'
#' @keywords methods
#' @export
coef.betasandwich <- function(object,
                              ...) {
  object$est
}

#' Confidence Intervals for Standardized Regression Slopes
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inherit summary.betasandwich details
#'
#' @return Returns a matrix of confidence intervals.
#'
#' @param object Object of class `betasandwich`.
#' @param ... additional arguments.
#' @param parm a specification of which parameters
#'   are to be given confidence intervals,
#'   either a vector of numbers or a vector of names.
#'   If missing, all parameters are considered.
#' @param level the confidence level required.
#'
#' @examples
#' # Data ---------------------------------------------------------------------
#' data("nas1982", package = "betaSandwich")
#'
#' # Fit Model in lm ----------------------------------------------------------
#' object <- stats::lm(
#'   formula = QUALITY ~ NARTIC + PCTGRT + PCTSUPP,
#'   data = nas1982
#' )
#'
#' # BetaHC -------------------------------------------------------------------
#' std <- BetaHC(object = object)
#'
#' ## Methods -----------------------------------------------------------------
#' confint(object = std, level = 0.95)
#'
#' @keywords methods
#' @export
confint.betasandwich <- function(object,
                                 parm = NULL,
                                 level = 0.95,
                                 ...) {
  if (is.null(x = parm)) {
    parm <- seq_len(length.out = object$lm_process$p)
  }
  ci <- .BetaCI(
    object = object,
    alpha = 1 - level[1]
  )[parm, 6:7, drop = FALSE] # always t
  varnames <- colnames(x = ci)
  varnames <- gsub(
    pattern = "%",
    replacement = " %",
    x = varnames
  )
  colnames(x = ci) <- varnames
  ci
}
