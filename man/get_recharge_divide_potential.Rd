% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/anem_recharge.R
\name{get_recharge_divide_potential}
\alias{get_recharge_divide_potential}
\title{Recharge divide potential function}
\usage{
get_recharge_divide_potential(loc, aquifer)
}
\arguments{
\item{loc}{Location for evaluating Vector location as c(x,y) or data.frame containing x, y columns}

\item{Aquifer}{Aquifer containing containing \code{aquifer_type},\code{h0} and \code{recharge} (see Details)}
}
\value{
Returns hydraulic head (confined aquifers) or discharge potential (unconfined aquifers) at the location(s).
}
\description{
Recharge divide potential function
}
\details{
Recharge parameters must contain \code{x0}, \code{y0}, \code{x_term}, and \code{y_term}. For confined aquifers,
the last two are:
\itemize{
\item \code{x_term}: -flow * cos(theta) * sign(dx) / (Ksat * z0)
\item \code{y_term}: -flow * sin(theta) * sign(dy) / (Ksat * z0)
}
For unconfined aquifers,
these parameters are:
\itemize{
\item \code{x_term}: -2 * flow * cos(theta) * sign(dx) / Ksat
\item \code{y_term}: -2 * flow * sin(theta) * sign(dy) / Ksat
}
}
\examples{
\dontrun{
## Flow - confined aquifer
recharge_params <- list(recharge_type="D",recharge_vector=c(0,0,1,sqrt(3)),
  flow_main=1,flow_opp=2,x0=0,y0=0)
aquifer <- define_aquifer("confined",1,h0=0,z0=1,recharge=recharge_params)
get_recharge_divide_potential(c(1/2,sqrt(3)/2), aquifer)
get_recharge_divide_potential(c(-1/2,-sqrt(3)/2), aquifer)

loc <- expand.grid(x=-2:2,y=-2:2)
loc$h <- get_recharge_divide_potential(loc, aquifer)
library(ggplot2)
ggplot(loc) + geom_raster(aes(x,y,fill=h)) + scale_fill_gradient2()

recharge_params <- list(recharge_type="D",recharge_vector=c(-1,-5,0,0),
  flow_main=1,flow_opp=1,x0=0,y0=0)
aquifer <- define_aquifer("confined",1,h0=0,z0=10,recharge=recharge_params)

loc <- data.frame(x=c(0,0),y=c(5,6))
get_recharge_divide_potential(loc, aquifer)
loc <- expand.grid(x=-10:10,y=-10:10)
loc$h <- get_recharge_divide_potential(loc, aquifer)
ggplot(loc) + geom_raster(aes(x,y,fill=h)) + scale_fill_gradient2()

recharge_params <- list(recharge_type="D",recharge_vector=c(0,0,1,1),
  flow_main=1,flow_opp=1,x0=0,y0=0)
aquifer <- define_aquifer("confined",1e-1,h0=0,z0=10,recharge=recharge_params)
loc <- c(10,10)
get_recharge_divide_potential(c(-5,5), aquifer)
loc <- expand.grid(x=-10:10,y=-10:10)
loc$h <- get_recharge_divide_potential(loc, aquifer)
ggplot(loc) + geom_raster(aes(x,y,fill=h)) + scale_fill_gradient2(midpoint=0)

recharge_params <- list(recharge_type="D",recharge_vector=c(0,0,1,0),
  flow_main=1,flow_opp=1,x0=0,y0=0)
aquifer <- define_aquifer("unconfined",1e-3,h0=1e3,recharge=recharge_params)
loc <- c(0,2)
get_recharge_divide_potential(c(2,0), aquifer)
loc <- expand.grid(x=-10:10,y=-10:10)
loc$h2 <- get_recharge_divide_potential(loc, aquifer)
loc <- loc \%>\% dplyr::mutate(h=sqrt(h2))
ggplot(loc) + geom_raster(aes(x,y,fill=h)) + scale_fill_gradient2(midpoint=aquifer$h0)
}
}
\keyword{internal}
