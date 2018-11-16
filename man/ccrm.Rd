\name{ccrm}
\alias{ccrm}
\alias{ccrm.default}
\alias{ccrm.formula}
\title{Constrained Centre and Range Method}
\description{ \code{ccrm} is used to fit a linear regression model to symbolic interval-valued variables based on the inequality constraints over
the range variables (Lima Neto and De Carvalho, 2010).}
\usage{
ccrm(formula1, formula2, data, ...)
}
\arguments{
  \item{formula1}{ an object of class "\code{\link{formula}}": the description of the first model to be fitted. }
  \item{formula2}{ an object of class "\code{\link{formula}}": the description of the second model to be fitted. }
  \item{data}{ an optional data frame containing the variables in the model. }
  \item{\dots}{ other arguments. }  
}
\details{
The Constrained Centre and Range method (CCRM) was proposed by Lima Neto and De Carvalho (2010) and fits two independent linear 
regression models on the midpoint and range of the intervals. In the Constrained Centre and Range Method, the estimative of the parameters
of the range's model is based on inequality constraints. There is no constraints over the parameters estimates for the midpoint regression equation.
The aim is to guarantee mathematical coherence between the
predicted values of the lower and upper bounds of the response interval-valued variable Y, i.e., yL < yU.

}
\value{
	\code{ccrm} returns an object of class "\code{ccrm}" including at least the following elements:
	\item{coefficients.C}{ a named vector of coefficients for the Centre's explanatory variables. }
	\item{coefficients.R}{ a named vector of coefficients for the Range's explanatory variables.}
	\item{sigma.C}{ an estimative of the standard deviation for the Centre's response variable.}
	\item{sigma.R}{ an estimative of the standard deviation for the Range's response variable.}
	\item{df.C}{ the degrees of freedom for the Centre residuals}
	\item{df.R}{ the degrees of freedom for the Range residuals}
	\item{fitted.values.l}{ the fitted values for the lower interval bound. }
	\item{fitted.values.u}{ the fitted values for the upper interval bound.}
	\item{residuals.l}{ the ordinary residuals for the lower interval bound. }
	\item{residuals.u}{ the ordinary residuals for the upper interval bound.}
}
\references{

Lima Neto, E.A. and De Carvalho, F.A.T. (2010). Constrained linear regression models for
symbolic interval-valued variables. \emph{Computational Statistics and Data Analysis}, 54, 333--347.

}
\author{ Eufrasio de A. Lima Neto \email{eufrasio@de.ufpb.br} , Claudio A. V. de Souza Filho \email{cldsza@gmail.com}  }

\note{

\code{formula1} must contain the midpoint of the symbolic interval-valued variables. \code{formula2} contain the range (upper limit minus lower limit) of the symbolic interval-valued variables.

}

\seealso{
\code{\link{summary.ccrm}}, \code{\link{coef.ccrm}}, \code{\link{fitted.ccrm}}, \code{\link{residuals.ccrm}}, \code{\link{formula}}
}
\examples{
data("Cardiological.CR", package = "iRegression")
ex.ccrm <- ccrm(PulseC~SystC+DiastC,PulseR~SystR+DiastR,data=Cardiological.CR)
ex.ccrm
}
\keyword{ccrm}

