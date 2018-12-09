\name{summary.bivar}
\alias{summary.bivar}
\title{Summarizing Bivariate Symbolic Regression Method Fits}
\description{ \code{summary} method for class \code{\link{bivar}}.}
\usage{
\method{summary}{bivar}(object, ...)
}
\arguments{
	\item{object}{ an object of class "\code{bivar}", usually, a result of a call to \code{\link{bivar}}}.
	\item{\dots}{ other arguments. }
}

\value{
	The function \code{summary.bivar} returns the following elements, given an object of the class "\code{bivar}",
	\item{Coefficients1}{ a named vector of coefficients for the explanatory variables of the model "1". }
	\item{Coefficients2}{ a named vector of coefficients for the explanatory variables of the model "2". }
	\item{RMSE1}{ root mean square error for the model "1". }
	\item{RMSE2}{ root mean square error for the model "2". }
	\item{Rho}{ the estimative for the correlation coefficient between Y1 and Y2. }
	\item{Phi}{ the estimative of the dispersion parameter. }
	\item{D}{ the goodness-of-fit measure deviance for the current model. }
}
\references{ 

Lima Neto, E. A., Cordeiro, G. and De Carvalho, F.A.T. (2011). Bivariate symbolic regression models for 
interval-valued variables. \emph{Journal of Statistical Computation and Simulation (Print)}, 81, 1727--1744.

 }

\seealso{
\code{\link{bivar}}
}
\examples{
##-- Continuing the  bivar() example:
data("soccer.bivar", package = "iRegression")
ex.bivar <- bivar(yMin~t1Min+t2Min, "identity", yMax~t1Max+t2Max, "identity", data=soccer.bivar)
ex.sum <- summary(ex.bivar)
ex.sum
}
\keyword{summary} \keyword{bivar}