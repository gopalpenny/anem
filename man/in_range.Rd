% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/anem_geoprocessing.R
\name{in_range}
\alias{in_range}
\title{Test if x is in range}
\usage{
in_range(x, x1, x2)
}
\description{
Test if x is in range [x1,x2]
}
\examples{
\dontrun{
in_range(5,1,4)
in_range(5,6,4)
in_range(1:5,rep(4,5),c(4,4,5,6,7))
}
}
\keyword{internal}
