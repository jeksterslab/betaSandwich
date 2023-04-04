#' Print Method for an Object of Class `diffbetasandwich`
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a matrix of
#'   differences of standardized regression slopes,
#'   standard errors,
#'   test statistics,
#'   p-values,
#'   and
#'   confidence intervals.
#'
#' @param x Object of class `diffbetasandwich`.
#' @param ... additional arguments.
#' @param alpha Significance level.
#' @param digits Digits to print.
#'
#' @examples
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' std <- BetaHC(object)
#' diff <- DiffBetaSandwich(std)
#' print(diff)
#' @export
#' @keywords methods
print.diffbetasandwich <- function(x,
                                   alpha = c(0.05, 0.01, 0.001),
                                   digits = 4,
                                   ...) {
  cat("Call:\n")
  base::print(x$call)
  cat(
    "\nDifference between standardized regression coefficients with",
    toupper(x$fit$args$type),
    "standard errors:\n"
  )
  base::print(
    round(
      .DiffBetaCI(
        object = x,
        alpha = alpha
      ),
      digits = digits
    )
  )
}

#' Summary Method for an Object of Class `diffbetasandwich`
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a matrix of
#'   differences of standardized regression slopes,
#'   standard errors,
#'   test statistics,
#'   p-values,
#'   and
#'   confidence intervals.
#'
#' @param object Object of class `diffbetasandwich`.
#' @param ... additional arguments.
#' @param alpha Significance level.
#' @param digits Digits to print.
#'
#' @examples
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' std <- BetaHC(object)
#' diff <- DiffBetaSandwich(std)
#' summary(diff)
#' @export
#' @keywords methods
summary.diffbetasandwich <- function(object,
                                     alpha = c(0.05, 0.01, 0.001),
                                     digits = 4,
                                     ...) {
  cat("Call:\n")
  base::print(object$call)
  cat(
    "\nDifference between standardized regression coefficients with",
    toupper(object$fit$args$type),
    "standard errors:\n"
  )
  return(
    round(
      .DiffBetaCI(
        object = object,
        alpha = alpha
      ),
      digits = digits
    )
  )
}

#' Sampling Covariance Matrix of
#' Differences of Standardized Regression Slopes
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a matrix of the
#'   variance-covariance matrix
#'   of differences of standardized regression slopes.
#'
#' @param object Object of class `diffbetasandwich`.
#' @param ... additional arguments.
#'
#' @examples
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' std <- BetaHC(object)
#' diff <- DiffBetaSandwich(std)
#' vcov(diff)
#' @export
#' @keywords methods
vcov.diffbetasandwich <- function(object,
                                  ...) {
  return(
    object$vcov
  )
}

#' Differences of Standardized Regression Slopes
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a vector of differences of standardized regression slopes.
#'
#' @param object Object of class `diffbetasandwich`.
#' @param ... additional arguments.
#'
#' @examples
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' std <- BetaHC(object)
#' diff <- DiffBetaSandwich(std)
#' coef(diff)
#' @export
#' @keywords methods
coef.diffbetasandwich <- function(object,
                                  ...) {
  return(
    object$est
  )
}

#' Confidence Intervals for Differences
#' of Standardized Regression Slopes
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a matrix of confidence intervals.
#'
#' @param object Object of class `diffbetasandwich`.
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
#' diff <- DiffBetaSandwich(std)
#' confint(diff, level = 0.95)
#' @export
#' @keywords methods
confint.diffbetasandwich <- function(object,
                                     parm = NULL,
                                     level = 0.95,
                                     ...) {
  if (is.null(parm)) {
    parm <- seq_len(
      length(object$est)
    )
  }
  return(
    .DiffBetaCI(
      object = object,
      alpha = 1 - level[1]
    )[parm, 5:6]
  )
}
