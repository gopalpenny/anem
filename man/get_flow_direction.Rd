% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/anem_potentials.R
\name{get_flow_direction}
\alias{get_flow_direction}
\title{Calculate the flow direction}
\usage{
get_flow_direction(loc, wells, aquifer, show_progress = FALSE,
  eps = 1e-06)
}
\arguments{
\item{loc}{coordinates vector as c(x,y), with units of [m] or as data.frame with columns $x and $y}

\item{wells}{wells object with each row containing rate Q [m^3/s], diam [m],
radius of influence R [m], & coordinates x [m], y [m]}

\item{aquifer}{Afuifer object containing aquifer_type, h0, Ksat, bounds, z0 (for confined case only)}

\item{show_progress}{Boolean input parameter. If true and there are >20 combinations of
wells and locations, then a progress bar will be printed.}

\item{eps}{Threshold satisfying numeric derivative}
}
\value{
Outputs the flow direction in the x and y directions. If the input \code{loc} is
  a numeric \code{c(x,y)}, then the output is in the same format. If the input is a data.frame,
  then the output is also a data.frame with columns \code{dx} and \code{dy}. The two values
  indicate the flow direction, and are equivalent to \eqn{-dh/dx}
  and \eqn{-dh/dy}.
}
\description{
Calculates the derivative of hydraulic head in x and y directions, returning \eqn{-dh/dx}
  and \eqn{-dh/dy}. For confined aquifers, the result is calculated using the sum of the
  effect of each analytical element, whereas for unconfined aquifers the result can be calculated
  analytically or numerically from hydraulic head.
}
\examples{
wells <- define_wells(x=c(0,0.5),y=c(0,0.25),Q=c(1e-3,-2e-3),diam=c(0.75,0.8),R=c(300,300))
aquifer <- define_aquifer(h0=0,Ksat=0.00001,z0=30,aquifer_type="confined")
get_flow_direction(loc=c(5,5),wells,aquifer)

grid_pts <- expand.grid(x=seq(0,10,by=5),y=seq(0,10,by=5))
get_flow_direction(loc=grid_pts,wells,aquifer)

# Injection and pumping well along diagonal line
wells2 <- data.frame(x=c(-10,10),y=c(-10,10),Q=c(1e-3,-1e-3),diam=c(0.1,0.1),R=c(300,300))
grid_pts2 <- data.frame(x=c(-11,0,11),y=c(-11,0,11))
aquifer_unconfined <- define_aquifer(aquifer_type="unconfined",Ksat=0.00001,h0=20)
fd2 <- get_flow_direction(loc=grid_pts2,wells2,aquifer_unconfined)

# Two pumping wells along diagonal line
wells3 <- data.frame(x=c(-10,10),y=c(-10,10),Q=c(-1e-3,-1e-3),diam=c(0.1,0.1),R=c(300,300))
grid_pts3 <- data.frame(x=c(-3,-3,0,3,3,-2,2,-1,1),y=c(-3,3,0,-3,3,-1,1,-2,2))
fd3 <- get_flow_direction(wells3,loc=grid_pts3,aquifer_unconfined)

## plot the flow directions
# scale dx and dy for visualization
library(dplyr)
library(ggplot2)
library(magrittr)
fd3_grid <- grid_pts3 \%>\% cbind(fd3) \%>\%
   mutate(dx_norm=dx*50,dy_norm=dy*50,x2=x+dx_norm,y2=y+dy_norm) # normaliz
# alternatively, apply nonlinear scaling to dx and dy for visualization
fd3_grid <- grid_pts3 \%>\% cbind(fd3) \%>\%
mutate(angle=atan(dy/dx),mag=sqrt(dx^2+dy^2),mag_norm=mag^(1/2)*5,
       dx_norm=mag_norm*cos(angle)*sign(dx),dy_norm=mag_norm*sin(angle)*sign(dx),
       x2=x+dx_norm,y2=y+dy_norm)

ggplot(fd3_grid,aes(x,y)) + geom_point(size=2,shape=1) +
  geom_segment(aes(xend=x2,yend=y2),
    arrow=arrow(type="closed",length=unit(2,"mm"))) +
  coord_equal()


wells <- define_wells(Q=0.1,x=-2,y=-2,R=100,diam=0.5)
recharge_params <- list(recharge_type="F",recharge_vector=c(0,0,-1,-1),flow=1e-3,x0=0,y0=0)
aquifer <- define_aquifer("confined",1e-3,h0=0,z0=1,recharge=recharge_params)
get_flow_direction(c(-1,-1),wells,aquifer)
get_flow_direction(c(-1,-1),NULL,aquifer)

recharge_params <- list(recharge_type="D",recharge_vector=c(0,0,-1,-1),
  flow_main=1,flow_opp=1,x0=0,y0=0)
aquifer <- define_aquifer("confined",1,h0=0,z0=1,recharge=recharge_params)
loc <- expand.grid(x=-1:1,y=-1:1)
loc \%>\% bind_cols(get_flow_direction(loc,wells,aquifer))
loc \%>\% bind_cols(get_flow_direction(loc,NULL,aquifer))

recharge_params <- list(recharge_type="D",recharge_vector=c(10,10,11,11),
  flow_main=sqrt(2),flow_opp=sqrt(2),x0=0,y0=0)
aquifer <- define_aquifer("confined",1,h0=0,z0=1,recharge=recharge_params)
loc <- expand.grid(x=9:11,y=9:11)
get_flow_direction(loc,wells,aquifer)
get_flow_direction(loc,wells,aquifer)
}
