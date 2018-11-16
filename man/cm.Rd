\name{cm}
\alias{cm}
\alias{cm.default}
\alias{cm.formula}
\title{Centre Method}
\description{\code{cm} is used to fit a linear regression model to symbolic interval-valued variables based on the centre method (Billard and Diday, 2000).}
\usage{
cm(formula1, formula2, data, ...)
}
\arguments{
  \item{formula1}{ an object of class \code{\link{formula}}: a symbolic description of the model to be fitted. }
  \item{formula2}{ an object of class \code{\link{formula}}: a symbolic description of the model to be fitted. }
  \item{data}{ an optional data frame containing the variables in the model. }
  \item{\dots}{ other arguments. }
}
\details{
Billard and Diday (2000) presented the first approach to fitting a linear regression model to symbolic 
interval data sets from a SDA of view. Their approach consists on fitting a linear regression model to the mid-points of
the interval values assumed by the symbolic interval variables in the learning set and applies
this model to the lower and upper bounds of the interval values of the
independent symbolic interval variables to be predicted, respectively, the lower and upper bounds of
the interval value of the dependent variable.
The Centre Method is based on the minimization of the midpoint
error. The lower and upper bounds of the dependent variable are predicted, respectively,
from the lower and upper bounds of the independent variable using the same
vector of parameters \bold{\emph{beta}}.
}
\value{
	\code{cm} returns an object of class "\code{cm}" including at least the following elements:
	\item{coefficients}{ a named vector of coefficients. }
	\item{sigma}{ an estimate of standard deviation.}
	\item{df}{ the residual degrees of freedom. }
	\item{fitted.values.l}{ the fitted values for the lower interval bound. }
	\item{fitted.valuues.u}{ the fitted values for the upper interval bound.}
	\item{residuals.l}{ the ordinary residuals for the lower interval bound .}
	\item{residuals.u}{ the ordinary residuals for the upper interval bound . }
}	
\references{

Billard, L. and Diday, E. (2000) Regression analysis for interval-valued data. \emph{Data Analysis, Classification
and Related Methods: Proceedings of the Seventh Conference of the International
Federation of Classification Societies}, Springer-Verlag, pp. 369-374.

Lima Neto, E.A. and De Carvalho, F.A.T. (2008). Centre and range method to fitting a linear regression model on
symbolic interval data. \emph{Computational Statistics and Data Analysis}, \bold{52}, 1500--1515.

}
\author{Eufrasio de A. Lima Neto \email{eufrasio@de.ufpb.br} and Claudio A. V. de Souza Filho \email{cldsza@gmail.com}}

\note{
\code{formula1} must contain the lower limit of the symbolic interval-valued variables. \code{formula2} contain the upper limit
of the symbolic interval-valued variables.
}
\seealso{
\code{\link{summary.cm}}, \code{\link{coef}}, \code{\link{fitted.cm}}, \code{\link{residuals.cm}}, \code{\link{formula}}
}
\examples{
data("Cardiological.MinMax", package = "iRegression") ## see Billard and Diday (2000)
ex.cm <- cm(PulseMin~SystMin+DiastMin,PulseMax~SystMax+DiastMax,data=Cardiological.MinMax)
ex.cm

}
\keyword{cm}
