\name{summary.ccrm}
\alias{summary.ccrm}
\title{Summarizing Constrained Centre and Range Method Fits}
\description{ \code{summary} method for class \code{\link{ccrm}}.}
\usage{
\method{summary}{ccrm}(object, ...)
}
\arguments{
	\item{object}{ an object of class "\code{ccrm}", usually, a result of a call to \code{\link{ccrm}}. }
	\item{\dots}{ other arguments. }
}
\value{
	The function \code{summary.ccrm} returns the following elements, given an object of the class "\code{ccrm}",
	\item{Coef.C}{ a named vector of coefficients for the Centre explanatory variables. }
	\item{Coef.R}{ a named vector of coefficients for the Range explanatory variables. }
	\item{RMSE.l}{ root mean square error for the lower bound. }
	\item{RMSE.u}{ root mean square error for the upper bound. }
}
\references{

Lima Neto, E.A. and De Carvalho, F.A.T. (2010). Constrained linear regression models for
symbolic interval-valued variables. \emph{Computational Statistics and Data Analysis}, 54, 333--347.

}

\seealso{
\code{\link{ccrm}}
}

\examples{
##-- Continuing the  ccrm() example:
data("Cardiological.CR", package = "iRegression")
ex.ccrm <- ccrm(PulseC~SystC+DiastC,PulseR~SystR+DiastR,data=Cardiological.CR)
ex.sum <- summary(ex.ccrm)
ex.sum
}
\keyword{summary} \keyword{ccrm}