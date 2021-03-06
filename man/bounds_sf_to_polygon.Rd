% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/anem_geoprocessing.R
\name{bounds_sf_to_polygon}
\alias{bounds_sf_to_polygon}
\title{Bounds to polygon}
\usage{
bounds_sf_to_polygon(bounds_sf)
}
\arguments{
\item{bounds_sf}{An sf object containing 4 line segments that meet at the ends to
form a polygon}
}
\description{
Convert bounds as sf object to polygon
}
\examples{
\dontrun{
gw_district <- import_app_rds(params=anem::groundwater_district)
bounds_sf <- gw_district$aquifer$bounds
bounds_sf_to_polygon(bounds_sf)
}
}
\keyword{internal}
