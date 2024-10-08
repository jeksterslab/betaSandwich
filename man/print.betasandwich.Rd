% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/betaSandwich-methods-beta-sandwich.R
\name{print.betasandwich}
\alias{print.betasandwich}
\title{Print Method for an Object of Class \code{betasandwich}}
\usage{
\method{print}{betasandwich}(x, alpha = NULL, digits = 4, ...)
}
\arguments{
\item{x}{Object of class \code{betasandwich}.}

\item{alpha}{Numeric vector.
Significance level \eqn{\alpha}.
If \code{alpha = NULL},
use the argument \code{alpha} used in \code{x}.}

\item{digits}{Digits to print.}

\item{...}{additional arguments.}
}
\value{
Prints a matrix of
standardized regression slopes,
standard errors,
test statistics,
degrees of freedom,
p-values,
and
confidence intervals.
}
\description{
Print Method for an Object of Class \code{betasandwich}
}
\examples{
object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
std <- BetaHC(object)
print(std)

}
\author{
Ivan Jacob Agaloos Pesigan
}
\keyword{methods}
