\name{MinMax}
\alias{MinMax}
\alias{MinMax.default}
\alias{MinMax.formula}
\title{MinMax Method}
\description{\code{MinMax} is used to fit a linear regression model to symbolic interval-valued variables based on the MinMax method (Lima Neto and De Carvalho, 2008).}
\usage{
MinMax(formula1, formula2, data, ...)
}

\arguments{
  \item{formula1}{ an object of class "\code{\link{formula}}": a symbolic description of the model to be fitted. }
  \item{formula2}{ an object of class "\code{\link{formula}}": a symbolic description of the model to be fitted. }
  \item{data}{ an optional data frame containing the variables in the model. }
  \item{\dots}{ other arguments. }
}

\details{
The Min-Max Method suggests
to estimate the lower and upper bounds of the intervals
using different vectors of parameters. This is equivalent to supposing
independence between the values of lower and upper bounds of the intervals.
The MinMax Method fits two independent linear regression models on the lower and upper
bounds of the intervals, respectively, and minimizes the error of the lower bounds plus the
error of the upper bounds.
}

\value{
	\code{MinMax} returns an object of class "\code{MinMax}" including at least the following elements:
	\item{coefficients.l}{ a named vector of coefficients for the Min variables. }
	\item{coefficients.u}{ a named vector of coefficients for the Max variables. }
	\item{sigma.l}{ an estimate of standard deviation for the Min response variable}
	\item{sigma.u}{ an estimate of standard deviation for the Max response variable}
	\item{df.l}{ the degrees of freedom for the lower residuals}
	\item{df.u}{ the degrees of freedom for the upper residuals}
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
symbolic interval data. \emph{Computational Statistics and Data Analysis}, \bold{52}, 1500--1515.

}

\author{ Eufrasio de A. Lima Neto \email{eufrasio@de.ufpb.br} , Claudio A. V. de Souza Filho \email{cldsza@gmail.com}  }

\note{
\code{formula1} must contain the lower limit of the symbolic interval-valued variables. \code{formula2} contain the upper limit of the symbolic interval-valued variables.
}

\seealso{
\code{\link{summary.MinMax}}, \code{\link{coef.MinMax}}, \code{\link{fitted.MinMax}}, \code{\link{residuals.MinMax}}, \code{\link{formula}}
}

\examples{
data("Cardiological.MinMax", package = "iRegression") ## see Billard, L. and Diday, E. (2000)
ex.MinMax <- MinMax(PulseMin~SystMin+DiastMin,PulseMax~SystMax+DiastMax,data=Cardiological.MinMax)
ex.MinMax
}
\keyword{MinMax}
