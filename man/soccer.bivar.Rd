\name{soccer.bivar}
\alias{soccer.bivar}

\title{Soccer Interval Data Set}

\description{ A real interval-valued data set.
}

\usage{data("soccer.bivar")}

\format{
A data frame containing following variables:
\describe{
  \item{yMin}{ Minimum of the response variable Y (weight)}
  \item{t1Min}{ Minimum of the explanatory variable T1 (height)}
  \item{t2Min}{ Minimum of the explanatory variable T2 (age)}
  \item{yMax}{ Maximum of the response variable Y (weight)}
  \item{t1Max}{ Maximum of the explanatory variable T1 (height)}
  \item{t2Max}{ Maximum of the explanatory variable T2 (age)}
}
}

\details{ This data set concerns the record of the Weight (Y), Height (T1)
and Age (T2) from 20 soccer teams of the premiere French championship. }

\source{ Lima Neto et. al. (2011) }

\references{ 

Lima Neto, E. A., Cordeiro, G. and De Carvalho, F.A.T. (2011). Bivariate symbolic regression models for 
interval-valued variables. \emph{Journal of Statistical Computation and Simulation (Print)}, 81, 1727--1744.

 }

\seealso{\code{\link{cm}}, \code{\link{MinMax}}, \code{\link{bivar}}}

\examples{
data("soccer.bivar", package = "iRegression")

bivar1 <- bivar(yMin~t1Min+t2Min, "identity", yMax~t1Max+t2Max, "identity", data=soccer.bivar)
summary(bivar1)
}

\keyword{datasets}
