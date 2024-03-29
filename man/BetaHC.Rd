% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/betaSandwich-beta-hc.R
\name{BetaHC}
\alias{BetaHC}
\title{Estimate Standardized Regression Coefficients
and the Corresponding Robust Sampling Covariance Matrix
Using the Heteroskedasticity Consistent Approach}
\usage{
BetaHC(
  object,
  type = "hc3",
  alpha = c(0.05, 0.01, 0.001),
  g1 = 1,
  g2 = 1.5,
  k = 0.7
)
}
\arguments{
\item{object}{Object of class \code{lm}.}

\item{type}{Character string.
Correction type.
Possible values are
\code{"hc0"},
\code{"hc1"},
\code{"hc2"},
\code{"hc3"},
\code{"hc4"},
\code{"hc4m"}, and
\code{"hc5"}.}

\item{alpha}{Numeric vector.
Significance level \eqn{\alpha}.}

\item{g1}{Numeric.
\code{g1} value for \code{type = "hc4m"}.}

\item{g2}{Numeric.
\code{g2} value for \code{type = "hc4m"}.}

\item{k}{Numeric.
Constant \code{k} for \code{type = "hc5"}
\eqn{0 \leq k \leq 1}.}
}
\value{
Returns an object
of class \code{betasandwich} which is a list with the following elements:
\describe{
\item{call}{Function call.}
\item{args}{Function arguments.}
\item{lm_process}{Processed \code{lm} object.}
\item{gamma_n}{Asymptotic covariance matrix
of the sample covariance matrix
assuming multivariate normality.}
\item{gamma_hc}{Asymptotic covariance matrix
HC correction.}
\item{gamma}{Asymptotic covariance matrix
of the sample covariance matrix.}
\item{acov}{Asymptotic covariance matrix
of the standardized slopes.}
\item{vcov}{Sampling covariance matrix
of the standardized slopes.}
\item{est}{Vector of standardized slopes.}
}
}
\description{
Estimate Standardized Regression Coefficients
and the Corresponding Robust Sampling Covariance Matrix
Using the Heteroskedasticity Consistent Approach
}
\examples{
object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
std <- BetaHC(object)
# Methods -------------------------------------------------------
print(std)
summary(std)
coef(std)
vcov(std)
confint(std, level = 0.95)

}
\references{
Dudgeon, P. (2017).
Some improvements in confidence intervals
for standardized regression coefficients.
\emph{Psychometrika}, \emph{82}(4), 928–951.
\doi{10.1007/s11336-017-9563-z}

Pesigan, I. J. A., Sun, R. W., & Cheung, S. F. (2023).
betaDelta and betaSandwich:
Confidence intervals for standardized regression coefficients in R.
\emph{Multivariate Behavioral Research}.
\doi{10.1080/00273171.2023.2201277}
}
\seealso{
Other Beta Sandwich Functions: 
\code{\link{BetaADF}()},
\code{\link{BetaN}()},
\code{\link{DiffBetaSandwich}()},
\code{\link{RSqBetaSandwich}()}
}
\author{
Ivan Jacob Agaloos Pesigan
}
\concept{Beta Sandwich Functions}
\keyword{betaSandwich}
\keyword{std}
