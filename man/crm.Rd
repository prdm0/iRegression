\name{crm}
\alias{crm}
\alias{crm.default}
\alias{crm.formula}
\title{Centre and Range Method}
\description{ \code{crm} is used to fit a linear regression model to symbolic interval-valued variables based on the Centre and Range method (Lima Neto and De Carvalho, 2008).}
\usage{
crm(formula1, formula2, data, ...)
}
\arguments{
  \item{formula1}{ an object of class "\code{\link{formula}}": a symbolic description of the model to be fitted. }
  \item{formula2}{ an object of class "\code{\link{formula}}": a symbolic description of the model to be fitted. }
  \item{data}{ an optional data frame containing the variables in the model. }
  \item{\dots}{ other arguments. }  
}
\details{
In the Center Method, the estimate of the parameters
\bold{\emph{beta}} is based only on the midpoint of the intervals. However, the Centre and Range Method
proposed by Lima Neto and De Carvalho (2008) consider suitable to include
both the information given by the center and by the range of an interval-valued variable on a linear regression model to improve the
model prediction performance. The Centre and Range Method fits two independent linear regression models on the midpoint and range
of the intervals, respectively, and minimizes the error of the midpoint plus the error of the range.
}
\value{
	\code{cm} returns an object of class "\code{crm}" including at least the following elements:
	\item{coefficients.C}{ a named vector of coefficients for the Centre variables. }
	\item{coefficients.R}{ a named vector of coefficients for the Range variables.}
	\item{sigma.C}{ an estimate of standard deviation for the Centre response variable.}
	\item{sigma.R}{ an estimate of standard deviation for the Range response variable.}
	\item{df.C}{ the degrees of freedom for the centre residuals}
	\item{df.R}{ the degrees of freedom for the range residuals}
	\item{fitted.values.l}{ the fitted mean values for the lower interval bound. }
	\item{fitted.values.u}{ the fitted mean values for the upper interval bound.}
	\item{residuals.l}{ the residuals for the lower interval bound (that is response minus fitted values).}
	\item{residuals.u}{ the residuals for the upper interval bound (that is response minus fitted values).}
}
\references{

Billard, L. and Diday, E. (2000) Regression analysis for interval-valued data. \emph{Data Analysis, Classification
and Related Methods: Proceedings of the Seventh Conference of the International
Federation of Classification Societies}, Springer-Verlag, pp. 369-374.

Lima Neto, E.A. and De Carvalho, F.A.T. (2008). Centre and range method to fitting a linear regression model on
symbolic interval data. \emph{Computational Statistics and Data Analysis}, 52, 1500--1515.
}
\author{ Eufrasio de A. Lima Neto \email{eufrasio@de.ufpb.br} , Claudio A. V. de Souza Filho \email{cldsza@gmail.com}  }

\note{

\code{formula1} must contain the midpoint of the symbolic interval-valued variables. \code{formula2} contain the range (upper limit minus lower limit) of the symbolic interval-valued variables.

}

\seealso{
\code{\link{summary.crm}}, \code{\link{coef.crm}}, \code{\link{fitted.crm}}, \code{\link{residuals.crm}}, \code{\link{formula}}
}
\examples{
data("Cardiological.CR", package = "iRegression")
ex.crm <- crm(PulseC~SystC+DiastC,PulseR~SystR+DiastR,data=Cardiological.CR)
ex.crm
}
\keyword{crm}

