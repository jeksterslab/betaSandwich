#' Print Method for an Object of Class `rsqbetasandwich`
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a matrix of
#'   standardized regression slopes,
#'   standard errors,
#'   test statistics,
#'   p-values,
#'   and
#'   confidence intervals.
#'
#' @param x Object of class `rsqbetasandwich`.
#' @param ... additional arguments.
#' @param alpha Significance level.
#' @param digits Digits to print.
#'
#' @examples
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' std <- BetaHC(object)
#' rsq <- RSqBetaSandwich(std)
#' print(rsq)
#' @export
#' @keywords methods
print.rsqbetasandwich <- function(x,
                                  alpha = c(0.05, 0.01, 0.001),
                                  digits = 4,
                                  ...) {
  cat(
    "Multiple correlation with",
    toupper(x$fit$type),
    "standard errors:\n"
  )
  base::print(
    round(
      .RSqCI(
        object = x,
        alpha = alpha
      ),
      digits = digits
    )
  )
}

#' Summary Method for an Object of Class `rsqbetasandwich`
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a matrix of
#'   standardized regression slopes,
#'   standard errors,
#'   test statistics,
#'   p-values,
#'   and
#'   confidence intervals.
#'
#' @param object Object of class `rsqbetasandwich`.
#' @param ... additional arguments.
#' @param alpha Significance level.
#' @param digits Digits to print.
#'
#' @examples
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' std <- BetaHC(object)
#' rsq <- RSqBetaSandwich(std)
#' summary(rsq)
#' @export
#' @keywords methods
summary.rsqbetasandwich <- function(object,
                                    alpha = c(0.05, 0.01, 0.001),
                                    digits = 4,
                                    ...) {
  cat(
    "Multiple correlation with",
    toupper(object$fit$type),
    "standard errors:\n"
  )
  return(
    round(
      .RSqCI(
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
#' @param object Object of class `rsqbetasandwich`.
#' @param ... additional arguments.
#'
#' @examples
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' std <- BetaHC(object)
#' rsq <- RSqBetaSandwich(std)
#' vcov(rsq)
#' @export
#' @keywords methods
vcov.rsqbetasandwich <- function(object,
                                 ...) {
  return(
    .RSqCov(object)
  )
}

#' Differences of Standardized Regression Slopes
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a vector of differences of standardized regression slopes.
#'
#' @param object Object of class `rsqbetasandwich`.
#' @param ... additional arguments.
#'
#' @examples
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' std <- BetaHC(object)
#' rsq <- RSqBetaSandwich(std)
#' coef(rsq)
#' @export
#' @keywords methods
coef.rsqbetasandwich <- function(object,
                                 ...) {
  return(
    c(
      rsq = object$fit$lm_process$summary_lm$r.squared,
      adj = object$fit$lm_process$summary_lm$adj.r.squared
    )
  )
}

#' Confidence Intervals for Differences
#' of Standardized Regression Slopes
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
#' @export
#' @keywords methods
confint.rsqbetasandwich <- function(object,
                                    parm = NULL,
                                    level = 0.95,
                                    ...) {
  if (is.null(parm)) {
    parm <- seq_len(2)
  }
  return(
    .RSqCI(
      object = object,
      alpha = 1 - level[1]
    )[parm, 5:6]
  )
}
