# anem_particle_tracking.R

#' Get particle velocity
#'
#' Get particle velocity, in m/day
#' @param t time -- necessary for deSolve radau
#' @param loc loc as c(x,y)
#' @param params params for radau -- must included $wells, $aquifer$Ksat, $n, $direction (+1 or -1 for reverse)
#' @return
#' Returns the velocity of the particle in m/day
particle_velocity_m_day <- function(t, loc, params) {
  loc <- get_flow_direction(loc,params$wells,params$aquifer) * params$aquifer$Ksat / params$n * 3600 * 24 * params$direction
  return(list(loc))
}

#' Track particle in aquifer
#'
#' Track a particle in the aquifer from an original location to a pumping well,
#' aquifer boundary, or outside of the wells ROI. Coordinates must be in meters.
#' @param loc Coordinate vector as \code{c(x, y)} or \code{data.frame} with \code{x} and \code{y} columns
#' @param wells Wells \code{data.frame} object, containing well images
#' @param aquifer Aquifer as an \code{aquifer} object, with \code{Ksat}, porosity, \code{n}, and boundaries (which are used to calculate gridded velocities)
#' @param t_max Maximum time, in days, for which to calculate travel time
#' @param reverse If \code{TRUE}, particle tracking will run in reverse. Used for well capture zones
#' @param step_dist Determines the distance (m) to advance the particle each timestep. Can be set to "auto" or
#' any numeric value in m. If "auto", the distance is 1/2 the grid cell width. If numeric, should be smaller
#' than the grid spacing to ensure that particles are captured by wells.
#' @details
#' This function numerically integrates particle paths using the Euler method. The time step of integration depends on velocity.
#' Each time step is set so that the particle advances by a distance of step_dist (although there is a maximum time step 1 year).
#' Particle tracking continues as long as:
#' \itemize{
#' \item The particle has not encountered a well or boundary
#' \item The particle velocity is greater than 0
#' \item The total time is less than \code{t_max}
#' \item The particle has travelled less than step_dist * 1e5
#' }
#' The domain is discretized and velocities are calculated on a 200 x 200 grid. The instantaneous velocity for each time
#' step is calculated using bilinear interpolation. If a particle is near a source well (i.e., an injection well if \code{reverse = FALSE},
#' or a pumping well if \code{reverse = TRUE}), the velocity is calculated precisely at that location.
#'
#' Note: \code{get_capture_zone} does not work with \code{recharge_type == "D"}.
#' @return
#' Returns a data.frame containing the time and locations of particle. If \code{loc} is a \code{data.frame},
#' columns in \code{loc} not named x or y are included in results.
#' @importFrom magrittr %>%
#' @export
#' @examples
#' bounds_df <- data.frame(bound_type=c("NF","NF","CH","NF"),m=c(Inf,0,Inf,0),b=c(0,1000,1000,0))
#' aquifer <- define_aquifer(aquifer_type="confined",Ksat=0.001,n=0.4,h0=20,z0=20,bounds=bounds_df)
#' uncon_aquifer <- define_aquifer(aquifer_type="unconfined",Ksat=0.001,n=0.4,h0=20,bounds=bounds_df)
#' wells <- data.frame(x=c(400,100,650),y=c(300,600,800),Q=c(-1e-1,-1e-1,-1e-1),diam=c(1,1,1),R=c(500,100,600)) %>%
#'   define_wells() %>% generate_image_wells(aquifer)
#' gridded <- get_gridded_hydrodynamics(wells,aquifer,c(100,100),c(10,10))
#'
#' system.time(particle_path <- track_particles(loc=c(600,500), wells, aquifer, t_max = 365*2))
#' particle_path[nrow(particle_path),]
#' system.time(particle_path <- track_particles(loc=c(600,500), wells, uncon_aquifer, t_max = 365*2))
#' particle_path[nrow(particle_path),]
#'
#' loc <- data.frame(x=c(600,725,900,250,150,200),y=c(500,825,50,500,800,700)) %>% dplyr::mutate(p=letters[row_number()])
#' system.time(particle_path <- track_particles(loc, wells, aquifer, t_max=365*100))
#' particle_path %>% filter(status!="On path")
#' ggplot() +
#'   geom_raster(data=gridded$head,aes(x,y,fill=head_m)) +
#'   geom_segment(data=aquifer$bounds,aes(x1,y1,xend=x2,yend=y2,linetype=bound_type)) +
#'   geom_point(data=wells %>% dplyr::filter(wID==orig_wID),aes(x,y),shape=21) +
#'   geom_path(data=particle_path,aes(x,y,color=p)) +
#'   coord_equal()
track_particles <- function(loc, wells, aquifer, t_max = 365, reverse = FALSE, step_dist = "auto", grid_length=200) {
  if (any(aquifer$recharge$recharge_type == "D")) {
    stop("track_particles does not function with \"D\" type recharge")
  }
  # note: use profiling to evaluate code http://adv-r.had.co.nz/Profiling.html
  ca <- check_aquifer(aquifer,standard_columns = c("Ksat","n"))
  if (ca != "Good") {
    stop("check_aquifer() failed.")
  }

  # prep variables
  flow_sign <- ifelse(!reverse,1,-1)
  terminal_well_pumping_sign <- ifelse(!reverse,-1,1)
  grid_wells <- wells %>% dplyr::filter(well_image=="Actual") %>%
    dplyr::mutate(terminal_val=sign(Q)*terminal_well_pumping_sign,
                  terminal_type=factor(terminal_val,levels=-1:1,labels=c("source","non-operational","terminal")))
  terminal_wells <- grid_wells %>% dplyr::filter(terminal_val==1)

  # set grid to aquifer bounds, +1 cell on each side
  xgrid <- seq(min(c(aquifer$bounds$x1,aquifer$bounds$x2)),max(c(aquifer$bounds$x1,aquifer$bounds$x2)),length.out=grid_length)
  xgrid <- c(2*xgrid[1]-xgrid[2],xgrid,xgrid[length(xgrid)] + xgrid[2] - xgrid[1])
  ygrid <- seq(min(c(aquifer$bounds$y1,aquifer$bounds$y2)),max(c(aquifer$bounds$y1,aquifer$bounds$y2)),length.out=grid_length)
  ygrid <- c(2*ygrid[1]-ygrid[2],ygrid,ygrid[length(ygrid)] + ygrid[2] - ygrid[1])
  gridvals <- expand.grid(x=xgrid,y=ygrid) %>% tibble::as_tibble()

  velocity_constant <- aquifer$Ksat / aquifer$n * 3600 * 24 * flow_sign

  # # get gridded velocities
  # if (aquifer$aquifer_type == "confined") {
  v_grid_m_day <- get_flow_direction(gridvals,wells,aquifer) * velocity_constant
  # } else if (aquifer$aquifer_type == "unconfined") {
  #   v_grid_m_day <- get_flow_direction(gridvals,wells,aquifer) * velocity_constant
  #   # head0 <- get_flow_direction(gridvals,wells,aquifer)
  #   # headdx <- get_flow_direction(gridvals %>% dplyr::mutate(x=x+1e-6),wells,aquifer)
  #   # headdy <- get_flow_direction(gridvals %>% dplyr::mutate(y=y+1e-6),wells,aquifer)
  # }

  # Identify grid cells (aquifer boundaries & wells) where the simulation should stop
  well_grid_pts_prep <-
    do.call(rbind,lapply(split(grid_wells,1:nrow(grid_wells)),
                         function(well,df) df[which.min(sqrt((df$x - well$x)^2 +
                                                               (df$y - well$y)^2))[1],c("x","y")] %>%
                           tibble::add_column(val=well$terminal_val), df=gridvals)) %>%
    dplyr::mutate(well_cell=TRUE)
  well_grid_pts <- well_grid_pts_prep %>% dplyr::group_by(x,y) %>%
    dplyr::summarize(well_val=dplyr::case_when(
      any(val==-1) & any(val==1)~-2,
      any(val==-1)~-1,
      any(val==1)~2
    )) %>% dplyr::group_by()
  if(shiny_running()) {
    print("well_grid_pts")
    print(class(well_grid_pts))
    print(head(well_grid_pts))
    print("gridvals")
    print(class(gridvals))
    print(head(gridvals))
  }
  sim_status <- gridvals %>% dplyr::left_join(well_grid_pts,by=c("x","y")) %>%
    dplyr::mutate(inside_aquifer_cell=check_point_in_aquifer(x,y,aquifer),
                  status_val=dplyr::case_when(
                    is.na(well_val) & !inside_aquifer_cell ~ 1e3,
                    is.na(well_val) & inside_aquifer_cell ~ 0,
                    well_val < 0 ~ -4e3,
                    well_val== 2 ~ 1e3,
                    TRUE ~ 0
                  ))

  # # mapping / debugging
  # v_map <- cbind(gridvals,v_grid_m_day)
  # ggplot(v_map) + geom_raster(aes(x,y,fill=dx))
  # ggplot(sim_status) + geom_raster(aes(x,y,fill=status_val))

  # set up the grid for lookup / interpolation of values
  v_x_grid <- matrix(v_grid_m_day$dx,nrow=length(xgrid))
  v_y_grid <- matrix(v_grid_m_day$dy,nrow=length(xgrid))
  status_grid <- matrix(sim_status$status_val,nrow=length(xgrid))

  # get dL
  if (step_dist=="auto") {
    dL <- min(c(xgrid[2]-xgrid[1],ygrid[2]-ygrid[1]))/2
  } else {
    dL <- step_dist
  }

  if (any(grepl("data.frame",class(loc)))) {
    n_particles <- nrow(loc)
  } else {
    n_particles <- 1
    loc <- data.frame(x=loc[1],y=loc[2])
  }

  for (i in 1:n_particles) {
    particle_i <- cbind(time=0,x=loc$x[i],y=loc$y[i]) # columns have to be in this order -- it's what is returned by deSolve
    last <- particle_i[nrow(particle_i),]
    j <- 0
    v <- Inf
    status_val <- akima::bilinear(xgrid,ygrid,status_grid,x0=last["x"],y0=last["y"])$z
    captured_in_source_cell <- FALSE

    # track particle_i until one of the conditions is reached:
    while (last["time"] < t_max & v != 0 & status_val <= 0 & !captured_in_source_cell & j < 1e5) {
      j <- j + 1
      # interpolate current velocity (provided particle is not in a source cell)
      if (status_val >= 0) {
        v_x <- akima::bilinear(xgrid,ygrid,v_x_grid,x0=last["x"],y0=last["y"])$z
        v_y <- akima::bilinear(xgrid,ygrid,v_y_grid,x0=last["x"],y0=last["y"])$z
      } else { # if particle is near a source well (status_val < 0), get precise velocity
        fd <- get_flow_direction(last[c("x","y")],wells,aquifer) * velocity_constant
        v_x <- fd[1]
        v_y <- fd[2]
      }
      # calculate speed (v) and timestep (dt)
      v <- sqrt(v_x^2 + v_y^2)
      dt <- min(dL / v, 365)

      # get particle_i movement
      dx <- v_x * dt
      dy <- v_y * dt

      # update particle_i location
      last <- last + c(dt,dx,dy)
      particle_i <- rbind(particle_i,last)
      status_val <- akima::bilinear(xgrid,ygrid,status_grid,x0=last["x"],y0=last["y"])$z

      # if inside source cell, check for well capture
      if (status_val < 0) {
        inside_well_capture <- all(min(c(pmax(abs(last["x"] - terminal_wells$x),abs(last["x"] - terminal_wells$y)),Inf)) < dL)
        if (inside_well_capture) {
          captured_in_source_cell <- TRUE
        }
      }
    }
    particle_df <- as.data.frame(particle_i)

    # get distance to objects
    d_bounds <- c(get_distance_to_bounds(as.vector(last[c("x","y")]), aquifer$bounds),Inf)
    d_wells <- c(sqrt((terminal_wells$x - last["x"])^2 + (terminal_wells$y - last["y"])^2),Inf)

    if (v==0) {
      particle_status <- "Zero velocity"
    } else if (suppressWarnings(particle_i[nrow(particle_i),"time"] >= t_max)) {
      particle_status <- "Max time reached"
    } else if (j >= 1e5) {
      warning("track_particle max iterations (j=1e5) reached.")
      particle_status <- "Max iterations reached"
    } else if (min(d_wells) <= min(d_bounds)) {
      particle_status <- "Reached well"
    } else if (min(d_wells) > min(d_bounds)) {
      particle_status <- "Reached boundary"
    }

    particle_i_path <- particle_i %>% tibble::as_tibble() %>% setNames(c("time","x","y")) %>%
      dplyr::mutate(status="On path",
                    endpoint=FALSE,
                    i=i) %>%
      dplyr::rename(time_days=time) %>%
      dplyr::bind_cols(loc %>% dplyr::select(-x,-y) %>% dplyr::slice(rep(i,nrow(particle_i))))
    particle_i_path$status[nrow(particle_i_path)] <- particle_status
    particle_i_path$endpoint[nrow(particle_i_path)] <- TRUE
    if (i == 1) {
      particle_paths <- particle_i_path
    } else {
      particle_paths <- rbind(particle_paths,particle_i_path)
    }
  }
  return(particle_paths)
}


#' Get flowlines in confined aquifer
#'
#' Get flowlines in confined aquifer
#' @param wells Wells object
#' @param aquifer Aquifer object
#' @param flow_dim Dimensions for generating raster for generating flowlines
#' @examples
#' # define aquifer
#' bounds_df <- data.frame(bound_type=c("NF","NF","NF","NF"),m=c(Inf,0,Inf,0),b=c(0,1000,1000,0))
#' aquifer <- define_aquifer("confined",1e-3,bounds=bounds_df,h0=100)
#'
#' # define wells and well images
#' wells_df <- data.frame(x=c(300,700),y=c(200,600),diam=1) %>%
#'   dplyr::mutate(R=1000,Q=-1/dplyr::n())
#' wells <- define_wells(wells_df) %>% generate_image_wells(aquifer)
#'
#' ggplot() +
#'   geom_segment(data=aquifer_unconfined$bounds,aes(x1,y1,xend=x2,yend=y2,color=bound_type)) +
#'   geom_point(data=wells %>% filter(wID==orig_wID),aes(x,y),shape=21) +
#'   geom_path(data=cl0,aes(x,y,color=as.factor(line))) +
#'   coord_equal()
get_confined_flowlines <- function(wells,aquifer,nominal_levels=40, flow_dim=c(100,100)) {

  # define grid groundaries
  grid_bounds <- get_quad_vertices(aquifer$bounds) %>% dplyr::filter(!is.na(x)) %>%
    dplyr::summarize(xmin=min(x),xmax=max(x),ymin=min(y),ymax=max(y))
  # define grid
  loc <- with(grid_bounds,tidyr::crossing(x=seq(xmin,xmax,length.out=flow_dim[1]),
                                                    y=seq(ymin,ymax,length.out=flow_dim[2])))
  # get stream function
  df <- loc %>%
    dplyr::bind_cols(streamfunction=get_stream_function(loc,wells,aquifer)) %>%
    dplyr::rename(z=streamfunction)

  cl0 <- get_contourlines(df,nlevels = nominal_levels) %>% tibble::as_tibble()

  y_unique <- sort(unique(loc$y))
  y_diff <- y_unique[2] - y_unique[1]
  wells_orig <- wells %>% dplyr::filter(wID==orig_wID)

  for (i in 1:dim(wells_orig)[1]) {
    well_i <- wells_orig[2,]

    a <- cl0 %>% dplyr::group_by(level,line) %>%
      dplyr::mutate(i=dplyr::row_number(),
                    x_i=x-well_i$x,
                    y_i=y-well_i$y,
                    theta=get_theta(x_i,y_i),
                    d2=lead(theta) - 2*theta + lag(theta),
                    d2_change = abs(d2-lag(d2)),
                    d2_flag = d2_change > 10*lag(d2_change))
    ggplot(a) + geom_line(aes(i,d2,color=as.factor(line)))
    ggplot(a) + geom_point(aes(x,y,group=as.factor(line))) +
      geom_point(data=a %>% dplyr::filter(d2_flag),aes(x,y,color=d2_change))
    ggplot(a) + geom_path(aes(i,d2_change,color=as.factor(line)))
    l_axis <- cl0 %>% dplyr::filter(abs(y-well_i$y) < y_diff,x < well_i$x)
  }
  cl <- cl0 %>% mutate(dist=sapply(y,function(a) min(abs(a-wells_constant_head$y)))) # %>% filter(dist>y_diff*2)
  cl <- cl %>% group_by(line,level) %>% mutate(min_dist = min(dist),max_y=max(y)) %>% dplyr::filter(min_dist < 2, max_y > 150)

  cl1 <- get_contourlines(df,levels = unique(cl$level)+1e-2)
  # ggplot() +
}

#' Get capture zone
#'
#' Get the capture zone for one or more wells
#' @inheritParams track_particles
#' @param wIDs wells at which to generate capture zones. Set to "all" or numeric value (or vector) containing wID for wells.
#' @param t_max number of days to run particle tracking
#' @param n_particles number of particles to initialize at each well
#' @param buff_m buffer radius (m) around each well at which particles are initialized
#' @param injection_wells if TRUE, particle tracking from injection wells is allowed. if FALSE, particle tracking from injection wells is prohibited.
#' @param ... Additional arguments passed to \code{track_particles}
#' @importFrom magrittr %>%
#' @export
#' @details
#' Tracking particles are initialized radially around each well at a distance of \code{buff_m}. These particles must be
#' initialized outside any grid cells that overlap the well, because particle velocities inside this cell will be incorrect.
#'
#' Note: \code{get_capture_zone} does not work with \code{recharge_type == "D"}.
#' @examples
#' bounds_df <- data.frame(bound_type=c("NF","NF","CH","NF"),m=c(Inf,0,Inf,0),b=c(0,1000,1000,0))
#' aquifer <- define_aquifer(aquifer_type="confined",Ksat=0.001,n=0.4,h0=0,z0=20,bounds=bounds_df)
#' wells_df_orig <- wells_example
#' wells_df_orig[4,"Q"] <- 0.25
#' wells <- define_wells(wells_df_orig) %>% generate_image_wells(aquifer)
#' particle_paths <- get_capture_zone(wells, aquifer, t_max = 365*10, wIDs = "all")
#' particle_paths %>% dplyr::filter(endpoint)
#'
#' ggplot() +
#'   geom_segment(data=aquifer$bounds,aes(x1,y1,xend=x2,yend=y2,linetype=bound_type)) +
#'   geom_path(data=particle_paths,aes(x,y,color=as.factor(wID),group=interaction(pID,wID))) +
#'   geom_point(data=wells %>% dplyr::filter(wID==orig_wID),aes(x,y,color=as.factor(wID)),size=3) +
#'   geom_point(data=particle_endpoint,aes(x,y,shape=status)) +
#'   scale_shape_manual(values=c(5,4,3,0,1)) +
#'   coord_equal()
get_capture_zone <- function(wells, aquifer, t_max = 365, wIDs = "all", n_particles = 8, buff_m = 20, injection_wells = FALSE, ...) {
  if (any(aquifer$recharge$recharge_type == "D")) {
    stop("get_capture_zone does not function with \"D\" type recharge")
  }
  theta <- seq(0,2*pi*(1 - 1/n_particles),2*pi/n_particles)
  if (identical(wIDs,"all")) {
    wIDs <- wells %>% dplyr::filter(well_image=="Actual") %>% dplyr::pull(wID) %>% unique()
  }
  pts <- data.frame(dx = cos(theta), dy = sin(theta)) %>% dplyr::mutate(pID = dplyr::row_number())
  wells_capture <- wells %>%
    dplyr::filter(wID %in% wIDs)
  particles_matrix <- wells_capture %>%
    tidyr::crossing(pts) %>%
    dplyr::mutate(xp = x + dx * buff_m,
           yp = y + dy * diam * buff_m) %>%
    dplyr::select(wID,pID,xp,yp,well_type)

  # run particle tracking on pumping wells
  particles_matrix_pumping <- particles_matrix %>%
    dplyr::filter(wID %in% wells_capture$wID[wells_capture$well_type == "Pumping"])
  if (nrow(particles_matrix_pumping) > 0) {
    particle_paths_pumping <- track_particles(particles_matrix_pumping %>% dplyr::rename(x=xp,y=yp),wells,aquifer, t_max = t_max,reverse=TRUE)#, ...)
  } else {
    particle_paths_pumping <- tibble::tibble()
  }

  # run particle tracking on injection wells
  particles_matrix_injection <- particles_matrix %>%
    dplyr::filter(wID %in% wells_capture$wID[wells_capture$well_type == "Injection"])
  if (nrow(particles_matrix_injection) > 0 & injection_wells) {
    particle_paths_injection <- track_particles(particles_matrix_injection %>% dplyr::rename(x=xp,y=yp),wells,aquifer, t_max = t_max,reverse=FALSE)#, ...)
  } else {
    particle_paths_injection <- tibble::tibble()
  }

  particle_paths <- dplyr::bind_rows(particle_paths_pumping,particle_paths_injection)

  return(particle_paths)
}

#' Get capture zone regions
#'
#' Get well capture zone regions
#' @param type specifies which results to return. One of \code{"all"}, \code{"paths"}, \code{"endpoints"}, or \code{"smoothed"}. See details.
#' @details
#' \itemize{
#'   \item "all": Returns a list containing all three named objects described below.
#'   \item "paths": Returns a data.frame of particle paths
#'   \item "endpoints": Returns the end location of each particle after tracking
#'   \item "smoothed": Returns a data.frame with smoothed path intersecting the \code{endpoints}
#' }
get_capture_zone_regions <- function(particle_paths, aquifer, ...) {
  stop("Function not yet complete)")

  pps <- particle_paths %>% dplyr::mutate(rn=dplyr::row_number()) %>% dplyr::filter(rn %% 3 == 0)
  xgrid <- seq(min(c(aquifer$bounds$x1,aquifer$bounds$x2)),max(c(aquifer$bounds$x1,aquifer$bounds$x2)),length.out=100)
  ygrid <- seq(min(c(aquifer$bounds$y1,aquifer$bounds$y2)),max(c(aquifer$bounds$y1,aquifer$bounds$y2)),length.out=100)
  time_days <- matrix(rep(NA,length(xgrid)*length(ygrid)),nrow=length(xgrid))
  wID <- matrix(rep(NA,length(xgrid)*length(ygrid)),nrow=length(xgrid))
  grid <- tidyr::crossing(x=xgrid,y=ygrid) %>% dplyr::mutate(time_days=NA,wID=NA,pID=NA)

  for (i in 1:length(xgrid)) {
    x <- grid$x[i]
    for (j in 1:length(ygrid)) {
      y <- grid$y[j]
      idx <- which.min((x-pps$x)^2 + (y-pps$y)^2)
      grid[i,c("time_days","wID","pID")] <- pps[idx,c("time_days","wID","pID")]
      time_days[i,j] <- grid$time_days[i]
      wID[i,j] <- grid$wID[i]
    }
  }
  return(0)
}


#' Get angle from x, y
#'
#' Get angle from x,y
#' @examples
#' get_theta(-1,0)
#' get_theta(1,0)
#' get_theta(-1,1)/pi
#' get_theta(0,1)/pi
#' get_theta(c(1,1,0,-1,-1),c(0,1,1,1,0))/pi
get_theta <- function(x, y) {
  theta <- atan(y/x) + pi*(x<0)*dplyr::case_when(
    y>=0~1,
    y<0~-1,
    TRUE~0)
  return(theta)
}
