#' Print Method for an Object of Class betaSandwich
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Object of class `betaSandwich`.
#' @param ... additional arguments.
#' @param alpha Significance level.
#' @param digits Digits to print.
#' @examples
#' object <- lm(rating ~ ., data = attitude)
#' std <- BetaHC(object)
#' print(std)
#' @export
#' @keywords methods
print.betaSandwich <- function(x,
                               alpha = c(0.05, 0.01, 0.001),
                               digits = 4,
                               ...) {
  op <- options(digits = digits)
  on.exit(options(op))
  cat("Call:\n")
  base::print(x$call)
  cat(
    "\nStandardized regression slopes with",
    toupper(x$type),
    "standard errors:\n"
  )
  if (x$type == "mvn") {
    z <- TRUE
  } else {
    z <- FALSE
  }
  base::print(
    .BetaCI(
      object = x,
      alpha = alpha
    )
  )
}

#' Summary of the Results of BetaHC
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object Object of class `betaSandwich`.
#' @param ... additional arguments.
#' @param alpha Significance level.
#' @param digits Digits to print.
#' @examples
#' object <- lm(rating ~ ., data = attitude)
#' std <- BetaHC(object)
#' summary(std)
#' @export
#' @keywords methods
summary.betaSandwich <- function(object,
                                 alpha = c(0.05, 0.01, 0.001),
                                 digits = 4,
                                 ...) {
  op <- options(digits = digits)
  on.exit(options(op))
  cat("Call:\n")
  base::print(object$call)
  cat(
    "\nStandardized regression slopes with",
    toupper(object$type),
    "standard errors:\n"
  )
  return(
    .BetaCI(
      object = object,
      alpha = alpha
    )
  )
}

#' Robust Sampling Covariance Matrix of the Standardized Regression Slopes
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object Object of class `betaSandwich`.
#' @param ... additional arguments.
#' @examples
#' object <- lm(rating ~ ., data = attitude)
#' std <- BetaHC(object)
#' vcov(std)
#' @export
#' @keywords methods
vcov.betaSandwich <- function(object,
                              ...) {
  p <- length(object$beta)
  out <- object$beta.vcov[1:p, 1:p, drop = FALSE]
  rownames(out) <- colnames(out) <- names(object$beta)
  return(out)
}

#' Standardized Regression Slopes
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object Object of class `betaSandwich`.
#' @param ... additional arguments.
#' @examples
#' object <- lm(rating ~ ., data = attitude)
#' std <- BetaHC(object)
#' coef(std)
#' @export
#' @keywords methods
coef.betaSandwich <- function(object,
                              ...) {
  object$beta
}

#' Robust Confidence Intervals for Standardized Regression Slopes
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object Object of class `betaSandwich`.
#' @param ... additional arguments.
#' @param parm 	a specification of which parameters are to be given confidence intervals, either a vector of numbers or a vector of names. If missing, all parameters are considered.
#' @param level the confidence level required.
#' @examples
#' object <- lm(rating ~ ., data = attitude)
#' std <- BetaHC(object)
#' confint(std, level = 0.95)
#' @export
#' @keywords methods
confint.betaSandwich <- function(object,
                                 parm = NULL,
                                 level = 0.95,
                                 ...) {
  if (is.null(parm)) {
    parm <- 1:object$p
  }
  return(
    .BetaCI(
      object = object,
      alpha = 1 - level[1]
    )[parm, 5:6]
  )
}
