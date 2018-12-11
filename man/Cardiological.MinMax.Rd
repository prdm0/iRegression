\name{Cardiological.MinMax}
\alias{Cardiological.MinMax}

\title{Cardiological Interval Data Set}

\description{ A real interval-valued data set.
}

\usage{data("Cardiological.CR")}

\format{
A data frame containing following variables:
\describe{
  \item{PulseMin}{ Lower bound of the response interval-valued variable Pulse}
  \item{SystMin}{ Lower bound of the explanatory interval-valued variable Systolic Pressure}
  \item{DiastMin}{ Lower bound of the explanatory interval-valued variable Diastolic Pressure}
  \item{PulseMax}{  Upper bound of the response interval-valued variable Pulse}
  \item{SystMax}{ Upper bound of the explanatory interval-valued variable Systolic Pressure}
  \item{DiastMax}{ Upper bound of the explanatory interval-valued variable Diastolic Pressure}
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

\seealso{\code{\link{icm}}, \code{\link{MinMax}}}

\examples{
data("Cardiological.MinMax", package = "iRegression")

cm1 <- icm(PulseMin~SystMin+DiastMin,PulseMax~SystMax+DiastMax,data=Cardiological.MinMax)
summary(cm1)

##
data("Cardiological.MinMax", package = "iRegression")

MinMax1 <- MinMax(PulseMin~SystMin+DiastMin,PulseMax~SystMax+DiastMax,data=Cardiological.MinMax)
summary(MinMax1)
}

\keyword{datasets}
