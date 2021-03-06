% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/anem_geoprocessing.R
\name{get_intersection}
\alias{get_intersection}
\title{Get intersection of two lines}
\usage{
get_intersection(m1, b1, m2, b2)
}
\description{
Get intersection of two lines specified by slope m and intercept, b
}
\examples{
\dontrun{
get_intersection(1,0,-1,2)
get_intersection(Inf,1,0,2)
get_intersection(Inf,1,Inf,2)
}
}
\keyword{internal}
