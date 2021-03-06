% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/anem_geoprocessing.R
\name{prep_bounds_sf}
\alias{prep_bounds_sf}
\title{Convert boundaries sf object to dataframe, with appropriate column coordinates}
\usage{
prep_bounds_sf(bounds_sf)
}
\arguments{
\item{bounds_sf}{boundaries sf object, with line features (2 end points)}
}
\value{
outputs a data.frame with all of the attributes and coordinate endpoints as x1, y1, x2, y2.
}
\description{
Convert boundaries sf object to dataframe, with appropriate column coordinates
}
\keyword{internal}
