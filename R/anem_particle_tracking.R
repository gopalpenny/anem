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
  loc <- get_flowdir(loc,params$wells,params$aquifer) * params$aquifer$Ksat / params$n * 3600 * 24 * params$direction
  return(list(loc))
}

#' Track particle in aquifer
#'
#' Track a particle in the aquifer from an original location to a pumping well,
#' aquifer boundary, or outside of the wells ROI. Coordinates must be in meters.
#' @param loc Coordinate vector as \code{c(x, y)}
#' @param wells Wells \code{data.frame} object, containing well images.
#' @param aquifer Aquifer as an \code{aquifer} object, with \code{Ksat} and porosity, \code{n}
#' @param t_max Maximum time, in days, for which to calculate travel time
#' @param reverse If \code{TRUE}, particle tracking will run in reverse. Used for well capture zones
#' @param dL Determines the distance (m) to advance the particle each timestep. Can be set to "auto" or
#' any numeric value in m. If "auto", the distance is 1/2 the grid cell width. If numeric, should be smaller
#' than the grid spacing to ensure that particles are captured by wells.
#' @details
#' This function numerically integrates particle paths using the Euler method. The time step of integration is
#' variable -- each time step is calculated so that the particle advances by a distance of dL.
#' Particle tracking continues as long as:
#' \itemize{
#' \item The particle has not encountered a well or boundary
#' \item The particle velocity is greater than 0
#' \item The total time is less than \code{t_max}
#' \item The particle has travelled less than dL * 1e5
#' }
#' The domain is discretized and velocities are calculated on a 200 x 200 grid. The instantaneous velocity for each time
#' step is calculated using bilinear interpolation.
#' @return
#' Returns a data.frame containing the time and locations of particle.
#' @export
#' @examples
#' bounds_df <- data.frame(bound_type=c("NF","NF","CH","NF"),m=c(Inf,0,Inf,0),b=c(0,1000,1000,0))
#' aquifer <- define_aquifer(aquifer_type="confined",Ksat=0.001,n=0.4,h0=0,z0=20,bounds=bounds_df)
#' wells <- data.frame(x=c(400,100,650),y=c(300,600,800),Q=c(-1e-1,-1e-1,1e-1),diam=c(1,1,1),R=c(500,100,600)) %>%
#'   define_wells() %>% generate_image_wells(aquifer)
#' gridded <- get_gridded_hydrodynamics(wells,aquifer,c(100,100),c(10,10))
#'
#' system.time(particle_path <- track_particle(loc=c(600,500), wells, aquifer))
#' particle_path[nrow(particle_path),]
#'
#' loc <- data.frame(x=c(600,725,900),y=c(500,825,50))
#' system.time(particle_path <- track_particle(loc, wells, aquifer))
#' ggplot() +
#'   geom_raster(data=gridded$head,aes(x,y,fill=head_m)) +
#'   geom_segment(data=aquifer$bounds,aes(x1,y1,xend=x2,yend=y2,color=bound_type)) +
#'   geom_point(data=wells %>% dplyr::filter(wID==orig_wID),aes(x,y),shape=21) +
#'   geom_path(data=particle_path,aes(x,y),color="red") +
#'   coord_equal()
track_particle <- function(loc, wells, aquifer, t_max = 365*10, reverse = FALSE, dL = "auto") {
  # note: use profiling to evaluate code http://adv-r.had.co.nz/Profiling.html
  ca <- check_aquifer(aquifer,standard_columns = c("Ksat","n"))
  if (ca != "Good") {
    stop("check_aquifer() failed.")
  }

  # prep variables
  flow_sign <- ifelse(!reverse,1,-1)
  terminal_well_pumping_sign <- ifelse(!reverse,-1,1)
  terminal_wells <- wells %>% dplyr::filter(well_image=="Actual", sign(Q)==terminal_well_pumping_sign)

  # set grid to aquifer bounds, +1 cell on each side
  xgrid <- seq(min(c(aquifer$bounds$x1,aquifer$bounds$x2)),max(c(aquifer$bounds$x1,aquifer$bounds$x2)),length.out=200)
  xgrid <- c(2*xgrid[1]-xgrid[2],xgrid,xgrid[length(xgrid)] + xgrid[2] - xgrid[1])
  ygrid <- seq(min(c(aquifer$bounds$y1,aquifer$bounds$y2)),max(c(aquifer$bounds$y1,aquifer$bounds$y2)),length.out=200)
  ygrid <- c(2*ygrid[1]-ygrid[2],ygrid,ygrid[length(ygrid)] + ygrid[2] - ygrid[1])
  gridvals <- expand.grid(x=xgrid,y=ygrid)

  # get gridded velocities
  if (aquifer$aquifer_type == "confined") {
    v_grid_m_day <- get_flowdir(gridvals,wells,aquifer) * aquifer$Ksat / aquifer$n * 3600 * 24 * flow_sign
  } else if (aquifer$aquifer_type == "unconfined") {
    v_grid_m_day <- get_flowdir(gridvals,wells,aquifer) * aquifer$Ksat / aquifer$n * 3600 * 24 * flow_sign
    # head0 <- get_flowdir(gridvals,wells,aquifer)
    # headdx <- get_flowdir(gridvals %>% dplyr::mutate(x=x+1e-6),wells,aquifer)
    # headdy <- get_flowdir(gridvals %>% dplyr::mutate(y=y+1e-6),wells,aquifer)
  }

  # Identify grid cells (aquifer boundaries & wells) where the simulation should stop
  well_grid_pts <-
    do.call(rbind,lapply(split(terminal_wells,1:nrow(terminal_wells)),
                         function(well,df) df[which.min(sqrt((df$x - well$x)^2 +
                                                               (df$y - well$y)^2))[1],c("x","y")], df=gridvals)) %>%
    mutate(well_cell=TRUE)
  stop_sim <- gridvals %>% dplyr::left_join(well_grid_pts,by=c("x","y")) %>%
    dplyr::mutate(inside_aquifer=check_point_in_aquifer(x,y,aquifer),
                  inside_well=!is.na(well_cell),
                  stop=!inside_aquifer | inside_well,
                  stop_val=ifelse(stop,1e3,0))

  # # mapping / debugging
  # v_map <- cbind(gridvals,v_grid_m_day)
  # ggplot(v_map) + geom_raster(aes(x,y,fill=dx))
  # ggplot(stop_sim) + geom_raster(aes(x,y,fill=stop_val))

  # set up the grid for lookup / interpolation of values
  v_x_grid <- matrix(v_grid_m_day$dx,nrow=length(xgrid))
  v_y_grid <- matrix(v_grid_m_day$dy,nrow=length(xgrid))
  stop_grid <- matrix(stop_sim$stop_val,nrow=length(xgrid))

  # get dL
  if (dL=="auto") {
    dL <- min(c(xgrid[2]-xgrid[1],ygrid[2]-ygrid[1]))/2
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
    stop_val <- 0

    # track particle_i until one of the conditions is reached:
    while (last["time"] < t_max & v != 0 & stop_val < 25 & j < 1e5) {
      j <- j + 1
      # get current velocity
      v_x <- akima::bilinear(xgrid,ygrid,v_x_grid,x0=last["x"],y0=last["y"])$z
      v_y <- akima::bilinear(xgrid,ygrid,v_y_grid,x0=last["x"],y0=last["y"])$z
      # calculate speed (v) and timestep (dt)
      v <- sqrt(v_x^2 + v_y^2)
      dt <- dL / v

      # get particle_i movement
      dx <- v_x * dt
      dy <- v_y * dt

      # update particle_i location
      last <- last + c(dt,dx,dy)
      particle_i <- rbind(particle_i,last)
      stop_val <- akima::bilinear(xgrid,ygrid,stop_grid,x0=last["x"],y0=last["y"])$z
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
                    id=i) %>%
      dplyr::rename(time_days=time)
    particle_i_path$status[nrow(particle_i_path)] <- particle_status
    if (i == 1) {
      particles <- particle_i_path
    } else {
      particles <- rbind(particles,particle_i_path)
    }
  }
  return(particle_path)
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
      dplyr::mutate(i=row_number(),
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

# df <- constant_head_boundary
# # df %>% tidyr::complete(x,y) %>% purrr::pluck("z") %>% filter(is.na(z)) %>% any()
# # df %>% tidyr::complete(x,y) %>% filter(is.na(z),!is.nan(z))
#
# a <- df %>% group_by(x) %>% arrange(x,y) %>% mutate(d=abs(z-lag(z))) %>% filter(d==max(d,na.rm=TRUE))
#
# cl0 <- get_contourlines(df,nlevels = 20)
# cl <- cl0 %>% mutate(dist=sapply(y,function(a) min(abs(a-wells_constant_head$y)))) # %>% filter(dist>y_diff*2)
# cl <- cl %>% group_by(line,level) %>% mutate(min_dist = min(dist),max_y=max(y)) %>% filter(min_dist < 2, max_y > 150)
#
# cl1 <- get_contourlines(df,levels = unique(cl$level)+1e-2)
# ggplot() +
#   geom_raster(data=constant_head_boundary,aes(x,y,fill=streamfunction),alpha=0.5) +
#   # geom_contour(data=constant_head_boundary,aes(x,y,z=head),bins=20,linetype="dashed",color="black") +
#   # geom_contour(data=constant_head_boundary,aes(x,y,z=streamfunction),bins=20,color="black") +
#   # geom_path(data=cl,aes(x,y,group=line,color=level)) +
#   geom_path(data=cl0,aes(x,y,group=line),color="red",alpha=0.5) +
#   geom_path(data=cl1,aes(x,y,group=line),color="black",alpha=0.5) +
#   # geom_point(data=wells_constant_head,aes(x,y,color=well_type),size=3,shape=21) +
#   # theme(legend.position=c(0.8,0.1),legend.title=element_blank()) +
#   coord_equal()
#
# a <- df %>% filter(x==0) %>% arrange(y)

#' Get capture zone
#'
#' Get the capture zone for one or more wells
#' @inheritParams track_particle
#' @param wID numeric value (or vector) containing wID for wells at which to generate capture zones.
#' @param t_max number of days to run particle tracking
#' @param n_particles number of particles to initialize for each well
#' @param type specifie which results to return. One of \code{"all"}, \code{"paths"}, \code{"endpoints"}, or \code{"smoothed"}. See details.
#' @importFrom magrittr %>%
#' @details
#' \itemize{
#'   \item "all": Returns a list containing all three named objects described below.
#'   \item "paths": Returns a data.frame of particle paths
#'   \item "endpoints": Returns the end location of each particle after tracking
#'   \item "smoothed": Returns a data.frame with smoothed path intersecting the \code{endpoints}
#' }
#' @examples
#' aquifer <- aquifer_example
#' wells <- define_wells(wells_example[c(3,4,5),]) %>% generate_image_wells(aquifer)
#' get_capture_zone(wells,aquifer, wIDs = c(1,2))
#' ggplot() + geom_point(data=wells,aes(x,y,color=wID))
get_capture_zone <- function(wells, aquifer, wIDs, t_max, n_particles = 8, type="all") {
  theta <- seq(0,2*pi*(1 - 1/n_particles),2*pi/n_particles)
  pts <- data.frame(dx = cos(theta), dy = sin(theta)) %>% dplyr::mutate(pID = dplyr::row_number())
  wells_capture <- wells %>%
    dplyr::filter(wID %in% wIDs)
  particles_matrix <- wells_capture %>%
    tidyr::crossing(pts) %>%
    dplyr::mutate(xp = x + dx * diam * 1.01,
           yp = y + dy * diam * 1.01) %>%
    dplyr::select(wID,pID,xp,yp) %>%
    as.matrix()

  start <- Sys.time()
  for (i in 1:nrow(particles_matrix)) {
    print(i)
    loc <- particles_matrix[i,c("xp","yp")]
    particle <- track_particle(loc, wells, aquifer, t_max = 1000, reverse=TRUE, method="radau_tmax") %>%
      dplyr::mutate(wID=particles_matrix[i,"wID"],pID=particles_matrix[i,"pID"])
    if (i == 1) {
      particle_path_df <- particle
      particle_endpoint <- particle[nrow(particle),]
    } else {
      particle_path_df <- particle_path_df %>% dplyr::bind_rows(particle)
      particle_endpoint <- particle_endpoint %>% dplyr::bind_rows(particle[nrow(particle),])
    }
    end <- Sys.time()
    (elapsed_time <- end-start)
    print(elapsed_time)
  }

  p_radau_tmax <- ggplot() +
    geom_segment(data=aquifer$bounds,aes(x1,y1,xend=x2,yend=y2,linetype=bound_type)) +
    geom_path(data=particle_path_df,aes(x,y,color=as.factor(wID),group=interaction(pID,wID))) +
    geom_point(data=wells %>% filter(wID==orig_wID),aes(x,y,color=as.factor(wID)),size=3) +
    geom_point(data=particle_endpoint,aes(x,y,shape=status)) +
    scale_shape_manual(values=c(3,4,5,0,1)) +
    coord_equal() + labs(caption = "radau_tmax method with travel time guess and t_max = 1000 days. elapsed time = 10.2 min")
  ggsave("figs_test/radau_tmax.png",p_radau_tmax,width=7,height=4)

  particle_endpoint %>% pull(time_days) %>% mean()

  # ggplot(particles) + geom_point(aes(xp,yp))

  # return list of particle endpoints
  # return list of particle paths
  # return polygons of capture zone using smoothr
  return(list(result="function not finished"))
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
