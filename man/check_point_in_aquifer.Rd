% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/anem_geoprocessing.R
\name{check_point_in_aquifer}
\alias{check_point_in_aquifer}
\title{Check point is in bounds}
\usage{
check_point_in_aquifer(x, y, aquifer)
}
\value{
Function returns \code{TRUE} if the point is inside aquifer boundaries or on the boundary. If the point is outside
the boundaries, returns \code{FALSE}.
}
\description{
Check to see if one or more points falls within aquifer boundaries.
}
\examples{
\dontrun{
aquifer <- aquifer_unconfined_example
x <- 100
y <- 100
x <- seq(0,100,by=50)
y <- seq(0,100,by=50)
check_point_in_aquifer(200,200,aquifer)
check_point_in_aquifer(-200,0,aquifer)
check_point_in_aquifer(seq(0,100,by=50),seq(0,100,by=50),aquifer)
check_point_in_aquifer(seq(0,1000,by=500),seq(0,100,by=500),aquifer)
x <- rep(500,3)
y <- seq(0,1000,by=500)
check_point_in_aquifer(x,yseq(0,1000,by=500),aquifer)
}
}
\keyword{internal}
