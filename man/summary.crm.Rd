\name{summary.crm}
\alias{summary.crm}
\title{Summarizing Centre and Range Method Fits}
\description{ \code{summary} method for class \code{\link{crm}}.}
\usage{
\method{summary}{crm}(object, ...)
}
\arguments{
	\item{object}{ an object of class "\code{crm}", usually, a result of a call to \code{\link{crm}}. }
	\item{\dots}{ other arguments. }
}
\value{
	The function \code{summary.crm} returns the following elements, given an object of the class "\code{crm}",
	\item{Coef.C}{ a named vector of coefficients for the Centre explanatory variables. }
	\item{Coef.R}{ a named vector of coefficients for the Range explanatory variables. }
	\item{RMSE.l}{ root mean square error for the lower bound. }
	\item{RMSE.u}{ root mean square error for the upper bound. }
}
\references{

Billard, L. and Diday, E. (2000) Regression analysis for interval-valued data. \emph{Data Analysis, Classification
and Related Methods: Proceedings of the Seventh Conference of the International
Federation of Classification Societies}, Springer-Verlag, pp. 369-374.

Lima Neto, E.A. and De Carvalho, F.A.T. (2008). Centre and range method to fitting a linear regression model on
symbolic interval data. Computational Statistics and Data Analysis, \bold{52}, 1500--1515.

}

\seealso{
\code{\link{crm}}
}

\examples{
##-- Continuing the  crm() example:
data("Cardiological.CR", package = "iRegression")
ex.crm <- crm(PulseC~SystC+DiastC,PulseR~SystR+DiastR,data=Cardiological.CR)
ex.sum <- summary(ex.crm)
ex.sum
}
\keyword{summary} \keyword{crm}