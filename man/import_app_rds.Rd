% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/anem_shiny_related.R
\name{import_app_rds}
\alias{import_app_rds}
\title{Import app rds}
\usage{
import_app_rds(path, params = NULL)
}
\arguments{
\item{path}{Path to rds file downloaded from web}

\item{params}{List objected loaded from the rds files}
}
\value{
Returns a list containing \code{aquifer}, \code{wells}, and \code{particles} that
were input into the web application.
}
\description{
Import rds output from anem-app
}
\details{
The rds files downloaded from the web application contain the raw data and map
click information needed for the app. This function converts those raw values
to UTM coordinates for processing with the R package.

Either \code{path} or \code{params} should be supplied to the function. If both
are specified, only \code{path} will be used.
}
\examples{
# using path
app <- import_app_rds("~/Downloads/anem_scenario.rds")

# using params
params <- readRDS("~/Downloads/anem_scenario.rds")
app <- import_app_rds(params=params)

# view the data
gridded <- get_gridded_hydrodynamics(app$wells,app$aquifer,c(80,80))
ggplot() +
  geom_raster(data=gridded$head,aes(x,y,fill=head_m)) +
  geom_segment(data=gridded$flow,aes(x,y,xend=x2,yend=y2),
               arrow = arrow(ends="last",type="closed",length=unit(1,"mm")),color="black") +
  geom_segment(data=aquifer$bounds,aes(x1,y1,xend=x2,yend=y2,linetype=bound_type)) +
  coord_equal()
}