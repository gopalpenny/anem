% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/anem_particle_tracking.R
\name{track_particles}
\alias{track_particles}
\title{Track particle in aquifer}
\usage{
track_particles(loc, wells, aquifer, t_max = 365, reverse = FALSE,
  step_dist = "auto", grid_length = 200)
}
\arguments{
\item{loc}{Coordinate vector as \code{c(x, y)} or \code{data.frame} with \code{x} and \code{y} columns}

\item{wells}{Wells \code{data.frame} object, containing well images}

\item{aquifer}{Aquifer as an \code{aquifer} object, with \code{Ksat}, porosity, \code{n}, and boundaries (which are used to calculate gridded velocities)}

\item{t_max}{Maximum time, in days, for which to calculate travel time}

\item{reverse}{If \code{TRUE}, particle tracking will run in reverse. Used for well capture zones}

\item{step_dist}{Determines the distance (m) to advance the particle each timestep. Can be set to "auto" or
any numeric value in m. If "auto", the distance is 1/2 the grid cell width. If numeric, should be smaller
than the grid spacing to ensure that particles are captured by wells.}

\item{grid_length}{The number of grid cells in each dimension (x,y) to calculate exact particle velocity.}
}
\value{
Returns a data.frame containing the time and locations of particle. If \code{loc} is a \code{data.frame},
columns in \code{loc} not named x or y are included in results.
}
\description{
Track a particle in the aquifer from an original location to a pumping well,
aquifer boundary, or outside of the wells ROI. Coordinates must be in meters.
}
\details{
This function numerically integrates particle paths using the Euler method. The time step of integration depends on velocity.
Each time step is set so that the particle advances by a distance of step_dist (although there is a maximum time step 1 year).
Particle tracking continues as long as:
\itemize{
\item The particle has not encountered a well or boundary
\item The particle velocity is greater than 0
\item The total time is less than \code{t_max}
\item The particle has travelled less than step_dist * 1e5
}
The domain is discretized and velocities are calculated on a 200 x 200 grid. The instantaneous velocity for each time
step is calculated using bilinear interpolation. If a particle is near a source well (i.e., an injection well if \code{reverse = FALSE},
or a pumping well if \code{reverse = TRUE}), the velocity is calculated precisely at that location.

Note: \code{get_capture_zone} does not work with \code{recharge_type == "D"}.
}
\examples{
bounds_df <- data.frame(bound_type=c("NF","NF","CH","NF"),
  m=c(Inf,0,Inf,0),b=c(0,1000,1000,0))
aquifer <- define_aquifer(aquifer_type="confined",Ksat=0.001,
  n=0.4,h0=20,z0=20,bounds=bounds_df)
uncon_aquifer <- define_aquifer(aquifer_type="unconfined",
  Ksat=0.001,n=0.4,h0=20,bounds=bounds_df)
wells_df <- data.frame(x=c(400,100,650),y=c(300,600,800),
  Q=c(-1e-1,-1e-1,-1e-1),diam=c(1,1,1),R=c(500,100,600))
wells <- generate_image_wells(define_wells(wells_df),aquifer)
gridded <- get_gridded_hydrodynamics(wells,aquifer,c(100,100),c(10,10))

particle_path <- track_particles(loc=c(600,500), wells, aquifer, t_max = 365*2)
particle_path[nrow(particle_path),]
particle_path <- track_particles(loc=c(600,500), wells, uncon_aquifer, t_max = 365*2)
particle_path[nrow(particle_path),]

loc <- data.frame(x=c(600,725,900,250,150,200),y=c(500,825,50,500,800,700))
loc$p <- letters[1:nrow(loc)]
particle_path <- track_particles(loc, wells, aquifer, t_max=365*100)
particle_path[particle_path$status!="On path",]

library(ggplot2)
ggplot() +
  geom_raster(data=gridded$head,aes(x,y,fill=head_m)) +
  geom_segment(data=aquifer$bounds,aes(x1,y1,xend=x2,yend=y2,linetype=bound_type)) +
  geom_point(data=wells[wells$wID==wells$orig_wID,],aes(x,y),shape=21) +
  geom_path(data=particle_path,aes(x,y,color=p),show.legend=FALSE) +
  coord_equal()
}
