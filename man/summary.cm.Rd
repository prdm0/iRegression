\name{summary.icm}
\alias{summary.icm}
\title{Summarizing Centre Method Fits}
\description{ \code{summary} method for class \code{\link{icm}}.}
\usage{
\method{summary}{icm}(object, ...)
}
\arguments{
	\item{object}{ an object of class "\code{icm}", usually, a result of a call to \code{\link{icm}}.}
	\item{\dots}{ other arguments. }
}
\value{
	The function \code{summary.icm} returns the following elements, given an object of the class "\code{icm}",
	\item{coefficients}{ a named vector of coefficients. }
	\item{RMSE.l}{ root mean square error for the lower interval bound. }
	\item{RMSE.u}{ root mean square error for the upper interval bound. }
}
\references{

Billard, L. and Diday, E. (2000) Regression analysis for interval-valued data. \emph{Data Analysis, Classification
and Related Methods: Proceedings of the Seventh Conference of the International
Federation of Classification Societies}, Springer-Verlag, pp. 369-374.

Lima Neto, E.A. and De Carvalho, F.A.T. (2008). Centre and range method to fitting a linear regression model on
symbolic interval data. Computational Statistics and Data Analysis, \bold{52}, 1500--1515.

}

\seealso{
\code{\link{icm}}
}

\examples{
##-- Continuing the  icm() example:
data("Cardiological.MinMax", package = "iRegression")
ex.icm <- icm(PulseMin~SystMin+DiastMin,PulseMax~SystMax+DiastMax,data=Cardiological.MinMax)
ex.sum <- summary(ex.icm)
ex.sum
}
\keyword{summary} \keyword{icm}