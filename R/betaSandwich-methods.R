#' Print Values
#'
#' @param object object.
#' @param ... additional arguments.
#' @export
#' @keywords methods
print <- function(object,
                  ...) {
  UseMethod("print")
}

#' @param object Object of class `betaSandwich`.
#' @param ... additional arguments.
#' @param alpha Significance level.
#' @param digits Digits for rounding.
#' @export
#' @keywords methods
print.betaSandwich <- function(object,
                               alpha = c(0.05, 0.01, 0.001),
                               digits = 4,
                               ...) {
  cat("Call:\n")
  print(object$call)
  cat(
    "\nStandardized regression slopes with",
    toupper(object$type),
    "standard errors:\n"
  )
  p <- length(object$beta)
  print(
    round(
      .CIWald(
        object$beta,
        se = sqrt(diag(object$beta.vcov)),
        theta = 0,
        alpha = alpha,
        z = FALSE,
        df = object$df
      ),
      digits = digits
    )
  )
}

#' Object Summaries
#'
#' @param object object.
#' @param ... additional arguments.
#' @export
#' @keywords methods
summary <- function(object,
                    ...) {
  UseMethod("summary")
}

#' @export
#' @keywords methods
summary.default <- function(object,
                            ...) {
  summary(object, ...)
}

#' Summary of the Results of BetaHC
#'
#' @param object Object of class `betaSandwich`.
#' @param ... additional arguments.
#' @param alpha Significance level.
#' @param digits Digits for rounding.
#' @export
#' @keywords methods
summary.betaSandwich <- function(object,
                                 alpha = c(0.05, 0.01, 0.001),
                                 digits = 4,
                                 ...) {
  print(
    object,
    alpha = alpha,
    digits = digits
  )
}

#' Calculate Variance-Covariance Matrix for a Fitted Model Object
#'
#' @param object object.
#' @param ... additional arguments.
#' @export
#' @keywords methods
vcov <- function(object,
                 ...) {
  UseMethod("vcov")
}

#' @export
#' @keywords methods
vcov.default <- function(object,
                         ...) {
  vcov(object, ...)
}

#' Robust Sampling Covariance Matrix of the Standardized Regression Slopes
#'
#' @param object Object of class `betaSandwich`.
#' @param ... additional arguments.
#' @export
#' @keywords methods
vcov.betaSandwich <- function(object,
                              ...) {
  p <- length(object$beta)
  out <- object$beta.vcov[1:p, 1:p, drop = FALSE]
  rownames(out) <- colnames(out) <- names(object$beta)
  out
}

#' Extract Model Coefficients
#'
#' @param object object.
#' @param ... additional arguments.
#' @export
#' @keywords methods
coef <- function(object,
                 ...) {
  UseMethod("coef")
}

#' @export
#' @keywords methods
coef.default <- function(object,
                         ...) {
  coef(object, ...)
}

#' Standardized Regression Slopes
#'
#' @param object Object of class `betaSandwich`.
#' @param ... additional arguments.
#' @export
#' @keywords methods
coef.betaSandwich <- function(object,
                              ...) {
  object$beta
}

#' Confidence Intervals for Model Parameters
#'
#' @param object object.
#' @param ... additional arguments.
#' @export
#' @keywords methods
confint <- function(object,
                    ...) {
  UseMethod("confint")
}

#' @export
#' @keywords methods
confint.default <- function(object,
                            ...) {
  confint(object, ...)
}

#' Robust Confidence Intervals for Standardized Regression Slopes
#'
#' @param object Object of class `betaSandwich`.
#' @param ... additional arguments.
#' @param level the confidence level required.
#' @param digits Digits for rounding.
#' @export
#' @keywords methods
confint.betaSandwich <- function(object,
                                 level = 0.95,
                                 digits = 4,
                                 ...) {
  p <- length(object$beta)
  round(
    .CIWald(
      object$beta,
      se = sqrt(diag(object$beta.vcov)),
      theta = 0,
      alpha = 1 - level[1],
      z = FALSE,
      df = object$df
    )[, 5:6],
    digits = digits
  )
}
