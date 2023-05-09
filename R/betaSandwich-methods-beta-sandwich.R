#' Print Method for an Object of Class `betasandwich`
#'
#' @author Ivan Jacob Agaloos Pesigan
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
#' @param x Object of class `betasandwich`.
#' @param ... additional arguments.
#' @param alpha Significance level.
#' @param digits Digits to print.
#'
#' @examples
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' std <- BetaHC(object)
#' print(std)
#'
#' @family Beta Sandwich Functions
#' @keywords methods
#' @export
print.betasandwich <- function(x,
                               alpha = c(0.05, 0.01, 0.001),
                               digits = 4,
                               ...) {
  cat("Call:\n")
  base::print(x$call)
  cat(
    "\nStandardized regression slopes with",
    toupper(x$args$type),
    "standard errors:\n"
  )
  base::print(
    round(
      .BetaCI(
        object = x,
        alpha = alpha
      ),
      digits = digits
    )
  )
}

#' Summary Method for an Object of Class `betasandwich`
#'
#' @author Ivan Jacob Agaloos Pesigan
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
#' @param alpha Significance level.
#' @param digits Digits to print.
#'
#' @examples
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' std <- BetaHC(object)
#' summary(std)
#'
#' @family Beta Sandwich Functions
#' @keywords methods
#' @export
summary.betasandwich <- function(object,
                                 alpha = c(0.05, 0.01, 0.001),
                                 digits = 4,
                                 ...) {
  cat("Call:\n")
  base::print(object$call)
  cat(
    "\nStandardized regression slopes with",
    toupper(object$args$type),
    "standard errors:\n"
  )
  return(
    round(
      .BetaCI(
        object = object,
        alpha = alpha
      ),
      digits = digits
    )
  )
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
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' std <- BetaHC(object)
#' vcov(std)
#'
#' @family Beta Sandwich Functions
#' @keywords methods
#' @export
vcov.betasandwich <- function(object,
                              ...) {
  return(
    object$vcov[
      seq_len(object$lm_process$p),
      seq_len(object$lm_process$p),
      drop = FALSE
    ]
  )
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
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' std <- BetaHC(object)
#' coef(std)
#'
#' @family Beta Sandwich Functions
#' @keywords methods
#' @export
coef.betasandwich <- function(object,
                              ...) {
  return(
    object$est
  )
}

#' Confidence Intervals for Standardized Regression Slopes
#'
#' @author Ivan Jacob Agaloos Pesigan
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
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' std <- BetaHC(object)
#' confint(std, level = 0.95)
#'
#' @family Beta Sandwich Functions
#' @keywords methods
#' @export
confint.betasandwich <- function(object,
                                 parm = NULL,
                                 level = 0.95,
                                 ...) {
  if (is.null(parm)) {
    parm <- seq_len(object$lm_process$p)
  }
  return(
    .BetaCI(
      object = object,
      alpha = 1 - level[1]
    )[parm, 6:7] # always t
  )
}
