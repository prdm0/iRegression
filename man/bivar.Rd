\name{bivar}
\alias{bivar}
\alias{bivar.default}
\alias{bivar.formula}
\title{Bivariate Symbolic Regression Method}
\description{ This function fits an bivariate regression model for interval-valued variables, based on bivariate exponential family
of distributions, and return the fitted values,
the residuals, rho, phi and the goodness-of-fit measure deviance}
\usage{
bivar(formula1, lig1, formula2, lig2, data, ...)
}

\arguments{
  \item{formula1}{ an object of class "\code{\link{formula}}": the description of the first model to be fitted. }
  \item{lig1}{ the link function to be considered in the first model: identity, inverse or log }
  \item{formula2}{ an object of class "\code{\link{formula}}": the description of the second model to be fitted. }
  \item{lig2}{ the link function to be considered in the second model: identity, inverse or log }
  \item{data}{ an optional data frame containing the variables in the model. }
  \item{\dots}{ other arguments. }
}

\details{
This function fits an bivariate regression model for interval-valued variables considering the bivariate
Gaussian distribution in the random component Y = [Y1, Y2]. It is possible consider any pair of interval features
for the bivariate random vector Y. For example, the lower and upper interval bounds or the midpoint and
the range of intervals, respectively. It also possible to choice different link functions (identity, inverse or log) to connect
the random variables Y1 and Y2 with the respective linear predictors.
}

\value{
	\code{bivar} returns an object of class "\code{bivar}" including at least the following elements:
	\item{coefficients1}{ a named vector of coefficients for the explanatory variables of the model "1". }
	\item{coefficients2}{ a named vector of coefficients for the explanatory variables of the model "2". }
	\item{fitted.values1}{ the fitted values for the response variable Y1 . }
	\item{fitted.values2}{ the fitted values for the response variable Y2.}
	\item{residuals1}{ the ordinary residual for the response variable Y1.}
	\item{residuals2}{ the ordinary residual for the response variable Y2.}
	\item{residual.deviance}{the global residual for the bivariate vector Y=[Y1, Y2].}
	\item{Rho}{ the estimative for the correlation coefficient between Y1 and Y2.}
	\item{Phi}{ the estimative of the dispersion parameter.}
	\item{D}{ the goodness-of-fit measure deviance for the current model.}
}

\references{

Lima Neto, E. A., Cordeiro, G. and De Carvalho, F.A.T. (2011). Bivariate symbolic regression models for 
interval-valued variables. \emph{Journal of Statistical Computation and Simulation (Print)}, 81, 1727--1744.

}

\author{ Eufrasio de A. Lima Neto \email{eufrasio@de.ufpb.br} , Claudio A. V. de Souza Filho \email{cldsza@gmail.com}  }

\note{
\code{lig1} and \code{lig2} must be "\code{identity}", "\code{inverse}" or "\code{log}" for identity, inverse or logarithmic link functions, respectively.
}

\seealso{
\code{\link{summary.bivar}}, \code{\link{coef.bivar}}, \code{\link{fitted.bivar}}, \code{\link{residuals.bivar}}, \code{\link{formula}}
}

\examples{
data("soccer.bivar", package = "iRegression")
ex.bivar <- bivar(yMin~t1Min+t2Min, "identity", yMax~t1Max+t2Max, "identity", data=soccer.bivar)
ex.bivar
}
\keyword{bivar}
