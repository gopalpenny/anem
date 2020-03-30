# anem_obsolete.R

#' Track particle in aquifer
#'
#' Track a particle in the aquifer from an original location to a pumping well,
#' aquifer boundary, or outside of the wells ROI. Coordinates must be in meters.
#' @param loc Coordinate vector as \code{c(x, y)}
#' @param wells Wells \code{data.frame} object, containing well images.
#' @param aquifer Aquifer as an \code{aquifer} object, with \code{Ksat} and porosity, \code{n}
#' @param t_max Maximum time, in days, for which to calculate travel time
#' @param reverse If \code{TRUE}, particle tracking will run in reverse. Used for well capture zones
#' @details
#' This function uses numerical integration to track a particle along its path. The particle continues tracking,
#' provided that:
#' \itemize{
#' \item The particle has not encountered a well or boundary
#' \item The particle velocity is greater than 0
#' \item The total time is less than \code{t_max}
#' }
#' @return
#' Returns a data.frame containing the time and locations
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' library(tidyverse)
#' bounds_df <- data.frame(bound_type=c("NF","NF","CH","NF"),m=c(Inf,0,Inf,0),b=c(0,1000,1000,0))
#' aquifer <- define_aquifer(aquifer_type="confined",Ksat=0.001,n=0.4,h0=0,z0=20,bounds=bounds_df)
#' wells <- data.frame(x=c(400,100,650),y=c(300,600,800),Q=c(-1e-1,-1e-1,1e-1),diam=c(1,1,1),R=c(500,100,600)) %>%
#'   define_wells() %>% generate_image_wells(aquifer)
#' gridded <- get_gridded_hydrodynamics(wells,aquifer,c(100,100),c(10,10))
#'
#' system.time(particle_path <- track_particle(loc=c(600,500), wells, aquifer,method="radau1"))
#' particle_path[nrow(particle_path),]
#' system.time(particle_path <- track_particle(loc=c(600,500), wells, aquifer,method="radau2"))
#' particle_path[nrow(particle_path),]
#' ggplot() +
#'   geom_raster(data=gridded$head,aes(x,y,fill=head_m)) +
#'   geom_segment(data=aquifer$bounds,aes(x1,y1,xend=x2,yend=y2,color=bound_type)) +
#'   geom_point(data=wells %>% dplyr::filter(wID==orig_wID),aes(x,y),shape=21) +
#'   geom_path(data=particle_path,aes(x,y),color="red") +
#'   coord_equal()
#'
#'
#' system.time(particle_path <- track_particle(c(725,825), wells, aquifer,method="radau1"))
#' particle_path[nrow(particle_path),]
#' system.time(particle_path <- track_particle(c(725,825), wells, aquifer,method="radau2"))
#' particle_path[nrow(particle_path),]
#' system.time(particle_path <- track_particle(c(725,825), wells, aquifer,method="rk4"))
#' particle_path[nrow(particle_path),]
#' ggplot() +
#'   geom_raster(data=gridded$head,aes(x,y,fill=head_m)) +
#'   geom_segment(data=aquifer$bounds,aes(x1,y1,xend=x2,yend=y2,color=bound_type)) +
#'   geom_point(data=wells %>% dplyr::filter(wID==orig_wID),aes(x,y),shape=21) +
#'   geom_path(data=particle_path,aes(x,y),color="red") +
#'   coord_equal()
#'
#' particle_path <- track_particle(c(725,825), wells, aquifer, t_max=100)
#' particle_path[nrow(particle_path),]
#'
#' system.time(particle_path <- track_particle(c(900,50), wells, aquifer))
#' particle_path[nrow(particle_path),]
#' }
track_particle_runge_kutta <- function(loc, wells, aquifer, t_max = 365*10, reverse = FALSE, method = "radau1") {

  # function for roots (ie, when to stop numerical integration) -- stop the integration if the particle enters a well
  rootfun <- function(t, x, params) {
    d_bounds <- get_distance_to_bounds(x, params$aquifer$bounds)
    d_wells <- sqrt((params$orig_wells$x - x[1])^2 + (params$orig_wells$y - x[2])^2)
    # ifelse(x[1] > 200,0,1)
    root <- ifelse(any(d_wells <= params$orig_wells$diam),0,1)
    return(root)
  }

  ca <- check_aquifer(aquifer,standard_columns = c("Ksat","n"))
  if (ca != "Good") {
    stop("check_aquifer() failed.")
  }

  # prep params
  orig_wells <- wells %>% dplyr::filter(wID==orig_wID)
  wells_no_diam <- wells %>% dplyr::mutate(diam=0)
  params <- list(wells=wells_no_diam, orig_wells=orig_wells, aquifer=aquifer, n=aquifer$n, direction=ifelse(reverse,-1,1))

  particle <- cbind(time=0,x=loc[1],y=loc[2]) # columns have to be in this order -- it's what is returned by deSolve
  last <- particle[nrow(particle),]
  first <- particle[nrow(particle),]
  v <- particle_velocity_m_day(0, last[c("x","y")], params)[[1]]
  i <- 1
  if (sum(abs(v)) > 0) {
    particle_status <- "On path"
    d_bounds <- get_distance_to_bounds(as.numeric(loc), params$aquifer$bounds)
    wells_to_keep <- wells_in_direction(loc, v, params$orig_wells)
    d_wells <- ifelse(wells_to_keep,
                      sqrt((params$orig_wells$x - loc[1])^2 + (params$orig_wells$y - loc[2])^2),
                      rep(Inf,length(wells_to_keep)))
  } else {
    particle_status <- "Zero velocity"
    d_wells <- Inf
    d_bounds <- Inf
  }


  # conditions to continue particle tracking:
  # 1. the distance of the particle from all wells and boundaries must be greater than 1 m
  # 2. the particle velocity must be greater than 0
  # 3. the total integration time is less than the specified number of years
  while (all(d_wells > params$orig_wells$diam/1) & d_bounds > 1 & sum(abs(v)) > 0 & particle_status == "On path" & i < 100) {
    # print(i)

    start_loc <- as.numeric(last[c("x","y")])
    min_dist <- min(d_wells,d_bounds)

    # get new travel time guess -- this is two times the shortest-path time to nearest object at current speed
    # travel_time_guess <- min_dist / sqrt(v[1]^2 + v[2]^2) # original specification. now multiply by 2 to speed things up if possible

    # get particle tracking based on first guess of travel time
    ########################################################
    ########################################################
    # ptm <- proc.time()
    if (method == "radau1") { # works
      message("Using method: radau1")
      travel_time_guess <- min_dist / sqrt(v[1]^2 + v[2]^2)
      new_times <- seq(last["time"],min(last["time"] + travel_time_guess, t_max),length.out = 50) ### TESTED AND WORKS (BUT SLOWLY)
      # radau chosen to allow a root function which stops integration when the particle reaches a well or boundary
      # if the time guess is multiplied by 2, the particle can apparently skip over boundaries -- e.g.,
      # particle_path <- track_particle(loc=c(600,500), wells, aquifer) with the examples in the documentation
      # -- jumps boundary
      new_particle <- deSolve::radau(start_loc, new_times, particle_velocity_m_day,
                                     parms=params, rootfunc = rootfun, atol = 1e-2)
      # can try testing other numerical integration methods...
      # new_particle <- deSolve::rk(start_loc, new_times, particle_velocity_m_day,
      #                                parms=params, rootfunc = rootfun)
      # proc.time() - ptm
    } else if (method == "radau2") {
      message("Using method: radau2")
      travel_time_guess <- min_dist * 2 / sqrt(v[1]^2 + v[2]^2) # multiply by 2 -- Radau seems to allow this
      new_times <- seq(last["time"],min(last["time"] + travel_time_guess, t_max),length.out = 50)
      new_particle <- deSolve::radau(start_loc, new_times, particle_velocity_m_day,
                                     parms=params, rootfunc = rootfun, atol = 1e-2)
    } else if (method == "radau_tmax") {
      # note: this version runs quickly over short time periods. But it failed in the simulation in figs_test -- I stopped the sim after running for 11.6 minutes
      message("Using method: radau_tmax")
      travel_time_guess <- t_max
      new_times <- seq(last["time"],min(last["time"] + travel_time_guess, t_max),length.out = 100)
      new_particle <- deSolve::radau(start_loc, new_times, particle_velocity_m_day,
                                     parms=params, rootfunc = rootfun, atol = 1e-2)
    } else if (method == "rk4") {
      message("Using method: radau_tmax")
      travel_time_guess <- min_dist / sqrt(v[1]^2 + v[2]^2) #
      new_times <- seq(last["time"],min(last["time"] + travel_time_guess, t_max),length.out = 100)
      new_particle <- deSolve::rk4(start_loc, new_times, particle_velocity_m_day,
                                   parms=params)
    } else if (FALSE) {
      new_particle <- deSolve::radau(start_loc, new_times, particle_velocity_m_day,
                                     parms=params, atol = 1e-2, events = list(func= rootfun, root= TRUE, terminalroot=1))
    } else if (FALSE) {
      new_particle <- deSolve::rk(start_loc, new_times, particle_velocity_m_day, method = "rk23bs",
                                  parms=params, atol = 1e-2, events = list(func= rootfun, root= TRUE))
    } else if (FALSE) {
      new_particle <- deSolve::ode(start_loc, new_times, particle_velocity_m_day,
                                   parms=params, atol = 1e-2, rootfun = rootfun, events = list(func= rootfun, root= TRUE))
    } else if (FALSE) {
      new_particle <- deSolve::rk(start_loc, new_times, particle_velocity_m_day,
                                  parms=params, rootfunc = rootfun, atol = 1e-2)
      identical(new_particle1,new_particle)
    } else {
      stop("method not appropriately specified")
    }
    ########################################################
    ########################################################

    particle <- rbind(particle,new_particle[-1,])
    last <- particle[nrow(particle),]

    # get distance to objects
    d_bounds <- get_distance_to_bounds(as.vector(last[c("x","y")]), params$aquifer$bounds)
    # d_wells <- sqrt((params$orig_wells$x - last["x"])^2 + (params$orig_wells$y - last["y"])^2) # old
    wells_to_keep <- wells_in_direction(last[c("x","y")], v, params$orig_wells)
    d_wells <- ifelse(wells_to_keep,
                      sqrt((params$orig_wells$x - last["x"])^2 + (params$orig_wells$y - last["y"])^2),
                      rep(Inf,length(wells_to_keep)))


    i <- i + 1
    v <- particle_velocity_m_day(0, last[c("x","y")], params)[[1]]

    if (any(d_wells < params$orig_wells$diam/1)) {
      particle_status <- "Reached well"
    } else if (d_bounds < 1) {
      particle_status <- "Reached boundary"
    } else if (sum(abs(v)) == 0) {
      particle_status <- "Zero velocity"
    } else if (suppressWarnings(particle[nrow(particle),"time"] >= t_max)) {
      particle_status <- "Max time reached"
    } else if (i >= 100) {
      warning("track_particle max iterations (i=100) reached.")
      particle_status <- "Max itrations reached"
    }
  }

  particle_path <- particle %>% tibble::as_tibble() %>% setNames(c("time","x","y")) %>% dplyr::mutate(status="On path")
  particle_path$status[nrow(particle_path)] <- particle_status
  return(particle_path %>% dplyr::rename(time_days=time))
}

#' Wells in direction
#'
#' Wells in direction
#' @details
#' This function identifies which wells are in the direction the particle is moving (determined by line perpendicular to particle velocity)
#' @examples
#' wells_in_direction(c(0,0),c(1,1),data.frame(x=c(-1,0,1),y=c(-1,0,1)))
#' wells_in_direction(c(0,0),c(-1,-1),data.frame(x=c(-1,0,1),y=c(-1,0,1)))
#' wells_in_direction(c(100,1),c(1,0),data.frame(x=c(99,100,101),y=c(0,0,1)))
#' wells_in_direction(c(100,1),c(0,1),data.frame(x=c(99,100,101,102),y=c(-1,0,1,2)))
wells_in_direction <- function(loc, v, wells) {
  div_line <- c(get_slope_intercept(loc[1],loc[2],m=-v[1]/v[2]),xsign=sign(v[1]),ysign=sign(v[2]))
  if (abs(div_line$m) != Inf) {
    keep_wells <- sign(wells$y - wells$x * div_line$m - div_line$b) != div_line$ysign * -1
  } else {
    keep_wells <- sign(wells$x - div_line$b) != div_line$xsign * -1
  }
  return(keep_wells)
}
