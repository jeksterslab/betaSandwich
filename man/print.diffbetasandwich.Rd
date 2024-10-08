% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/betaSandwich-methods-diff-beta-sandwich.R
\name{print.diffbetasandwich}
\alias{print.diffbetasandwich}
\title{Print Method for an Object of Class \code{diffbetasandwich}}
\usage{
\method{print}{diffbetasandwich}(x, alpha = NULL, digits = 4, ...)
}
\arguments{
\item{x}{Object of class \code{diffbetasandwich}.}

\item{alpha}{Numeric vector.
Significance level \eqn{\alpha}.
If \code{alpha = NULL},
use the argument \code{alpha} used in \code{x}.}

\item{digits}{Digits to print.}

\item{...}{additional arguments.}
}
\value{
Prints a matrix of
differences of standardized regression slopes,
standard errors,
test statistics,
degrees of freedom,
p-values,
and
confidence intervals.
}
\description{
Print Method for an Object of Class \code{diffbetasandwich}
}
\examples{
object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
std <- BetaHC(object)
diff <- DiffBetaSandwich(std)
print(diff)

}
\author{
Ivan Jacob Agaloos Pesigan
}
\keyword{methods}
