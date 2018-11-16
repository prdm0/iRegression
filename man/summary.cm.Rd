\name{summary.cm}
\alias{summary.cm}
\title{Summarizing Centre Method Fits}
\description{ \code{summary} method for class \code{\link{cm}}.}
\usage{
\method{summary}{cm}(object, ...)
}
\arguments{
	\item{object}{ an object of class "\code{cm}", usually, a result of a call to \code{\link{cm}}.}
	\item{\dots}{ other arguments. }
}
\value{
	The function \code{summary.cm} returns the following elements, given an object of the class "\code{cm}",
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
\code{\link{cm}}
}

\examples{
##-- Continuing the  cm() example:
data("Cardiological.MinMax", package = "iRegression")
ex.cm <- cm(PulseMin~SystMin+DiastMin,PulseMax~SystMax+DiastMax,data=Cardiological.MinMax)
ex.sum <- summary(ex.cm)
ex.sum
}
\keyword{summary} \keyword{cm}