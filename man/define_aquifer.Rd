% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/anem_preparation.R
\name{define_aquifer}
\alias{define_aquifer}
\title{Define aquifer}
\usage{
define_aquifer(aquifer_type, Ksat, ...)
}
\arguments{
\item{aquifer_type}{"confined" or "unconfined"}

\item{Ksat}{Saturated hydraulic conductivity}

\item{...}{Optional parameters including boundaries, recharge_params, and aquifer thickness}
}
\value{
This function returns an S3 "aquifer" object that behaves as a list and contains
 the named items:
\itemize{
 \item aquifer_type: "confined" or "unconfined"
 \item Ksat: Saturated hydraulic conductivity
 \item h0: Hydraulic head of the undisturbed aquifer (i.e., no pumping)
 \item z0: Thickness of the aquifer (applies only to confined aquifers)
 \item bounds: Boundaries of the aquifer, if any (generated using \code{define_boundaries()})
}
}
\description{
Define an aquifer as confined or unconfined and with saturated hydraulic conductivity.
Other optional parameters can be defined including boundaries and recharge.
}
\examples{
(aquifer <- define_aquifer("confined",1e-4))
(aquifer <- define_aquifer("confined",1e-4,h0=100,z0=10))

bounds_df1 <- data.frame(bound_type=c("CH","NF","NF","NF"),
  x1=c(0,10,13,1),y1=c(0,10,9,-1),x2=c(10,13,1,0),y2=c(10,9,-1,0))
aquifer_confined <- define_aquifer("confined",1e-3,bounds=bounds_df1,h0=100,z0=10)
print(aquifer_confined)

bounds_df2 <- data.frame(bound_type=c("CH","CH","NF","NF"),
  x1=c(0,0,10,10),y1=c(0,10,10,0),x2=c(0,10,10,0),y2=c(10,10,0,0))
aquifer_confined <- define_aquifer("unconfined",1e-3,bounds=bounds_df2,h0=100)
aquifer_confined

recharge_params <- list(recharge_type="F",recharge_vector=c(0,0,3,3),flow=1,x0=3,y0=3)
aquifer <- define_aquifer("confined",1e-3,h0=50,z0=10,recharge=recharge_params)
}
