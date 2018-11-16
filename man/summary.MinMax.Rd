\name{summary.MinMax}
\alias{summary.MinMax}
\title{Summarizing MinMax Method Fits}
\description{ \code{summary} method for class \code{\link{MinMax}}.}
\usage{
\method{summary}{MinMax}(object, ...)
}
\arguments{
	\item{object}{ an object of class "\code{MinMax}", usually, a result of a call to \code{\link{MinMax}}}.
	\item{\dots}{ other arguments. }
}

\value{
	The function \code{summary.MinMax} returns the following elements, given an object of the class "\code{MinMax}",
	\item{Coef.L}{ a named vector of coefficients for the Min explanatory variables. }
	\item{Coef.U}{ a named vector of coefficients for the Max explanatory variables. }
	\item{RMSE.l}{ root mean square error for the lower bound. }
	\item{RMSE.u}{ root mean square error for the upper bound. }
}
\references{

Billard, L. and Diday, E. (2000) Regression analysis for interval-valued data. \emph{Data Analysis, Classification
and Related Methods: Proceedings of the Seventh Conference of the International
Federation of Classification Societies}, Springer-Verlag, pp. 369-374.

Lima Neto, E.A. and De Carvalho, F.A.T. (2008). Centre and range method to fitting a linear regression model on
symbolic interval data. \emph{Computational Statistics and Data Analysis}, \bold{52}, 1500--1515.

}

\seealso{
\code{\link{MinMax}}
}
\examples{
##-- Continuing the  MinMax() example:
data("Cardiological.MinMax", package = "iRegression")
ex.MinMax <- MinMax(PulseMin~SystMin+DiastMin,PulseMax~SystMax+DiastMax,data=Cardiological.MinMax)
ex.sum <- summary(ex.MinMax)
ex.sum
}
\keyword{summary} \keyword{MinMax}