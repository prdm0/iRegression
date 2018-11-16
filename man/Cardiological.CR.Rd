\name{Cardiological.CR}
\alias{Cardiological.CR}

\title{Cardiological Interval Data Set (Centre and Range)}

\description{ A real interval-valued data set represented in terms of the centre and the range of the intervals.
}

\usage{data("Cardiological.CR")}

\format{
A data frame containing the following variables:.
\describe{
  \item{PulseC}{ The midpoint of the response interval-valued variable Pulse }
  \item{SystC}{ The midpoint of the explanatory interval-valued variable Systolic Pressure}
  \item{DiastC}{ The midpoint of the explanatory interval-valued variable Diastolic Pressure}
  \item{PulseR}{ The range of the response interval-valued variable Pulse}
  \item{SystR}{ The range of the explanatory interval-valued variable Systolic Pressure}
  \item{DiastR}{ The range of the explanatory interval-valued variable Diastolic Pressure}
}
}

\details{ This data set concerns the record of the pulse rate (Y), systolic blood pressure (X1)
and diastolic blood pressure (X2) from 11 patients.
}

\source{ Billard and Diday (2000)
}

\references{ 

Billard, L. and Diday, E. (2000) Regression analysis for interval-valued data. \emph{Data Analysis, Classification
and Related Methods: Proceedings of the Seventh Conference of the International
Federation of Classification Societies}, Springer-Verlag, pp. 369-374.

}

\seealso{\code{\link{crm}}}

\examples{
data("Cardiological.CR", package = "iRegression")

crm1 <- crm(PulseC~SystC+DiastC,PulseR~SystR+DiastR,data=Cardiological.CR)
summary(crm1)
}

\keyword{datasets}
