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
#' ggplot() +
#'   geom_raster(data=gridded$head,aes(x,y,fill=head_m)) +
#'   geom_segment(data=aquifer$bounds,aes(x1,y1,xend=x2,yend=y2,color=bound_type)) +
#'   geom_point(data=wells %>% dplyr::filter(wID==orig_wID),aes(x,y),shape=21) +
#'   geom_path(data=particle_path,aes(x,y),color="red") +
#'   coord_equal()
#'
#'
#' system.time(particle_path <- track_particle(c(725,825), wells, aquifer))
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
track_particle <- function(loc, wells, aquifer, t_max = 365*10, reverse = FALSE) {

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
  v <- particle_velocity_m_day(0, last[c("x","y")], params)[[1]]
  i <- 1
  if (sum(abs(v)) > 0) {
    particle_status <- "On path"
  } else {
    particle_status <- "Zero velocity"
  }

  d_bounds <- get_distance_to_bounds(as.numeric(loc), params$aquifer$bounds)
  wells_to_keep <- wells_in_direction(loc, v, params$orig_wells)
  d_wells <- ifelse(wells_to_keep,
                    sqrt((params$orig_wells$x - loc[1])^2 + (params$orig_wells$y - loc[2])^2),
                    rep(Inf,length(wells_to_keep)))

  # conditions to continue particle tracking:
  # 1. the distance of the particle from all wells and boundaries must be greater than 1 m
  # 2. the particle velocity must be greater than 0
  # 3. the total integration time is less than the specified number of years
  while (all(d_wells > params$orig_wells$diam/1) & d_bounds > 1 & sum(abs(v)) > 0 & particle_status == "On path" & i < 100) {
    # print(i)

    # get new travel time guess -- this is two times the shortest-path time to nearest object at current speed
    min_dist <- min(d_wells,d_bounds)
    # travel_time_guess <- min_dist / sqrt(v[1]^2 + v[2]^2) # original specification. now multiply by 2 to speed things up if possible
    travel_time_guess <- min_dist * 2 / sqrt(v[1]^2 + v[2]^2) # multiply by 2 -- Radau seems to allow this

    # get particle tracking based on first guess of travel time
    ########################################################
    ########################################################
    # ptm <- proc.time()
    start_loc <- as.numeric(last[c("x","y")])
    new_times <- seq(last["time"],min(last["time"] + travel_time_guess, t_max),length.out = 50) ### TESTED AND WORKS (BUT SLOWLY)
    if (TRUE) {
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
    } else {
      new_particle <- deSolve::radau(start_loc, new_times, particle_velocity_m_day,
                                     parms=params, atol = 1e-2, events = list(func= rootfun, root= TRUE, terminalroot=1))
      new_particle <- deSolve::rk(start_loc, new_times, particle_velocity_m_day, method = "rk23bs",
                                  parms=params, atol = 1e-2, events = list(func= rootfun, root= TRUE))
      new_particle <- deSolve::ode(start_loc, new_times, particle_velocity_m_day,
                                   parms=params, atol = 1e-2, rootfun = rootfun, events = list(func= rootfun, root= TRUE))
      new_particle <- deSolve::rk(start_loc, new_times, particle_velocity_m_day,
                                     parms=params, rootfunc = rootfun, atol = 1e-2)
      identical(new_particle1,new_particle)
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
    }
  }
  if(i>=100) {
    warning("track_particle max iterations (i=100) reached.")
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
get_confined_flowlines <- function(wells,aquifer,nominal_levels=20, flow_dim=c(100,100)) {

  grid_bounds <- get_quad_vertices(aquifer$bounds) %>% dplyr::filter(!is.na(x)) %>%
    dplyr::summarize(xmin=min(x),xmax=max(x),ymin=min(y),ymax=max(y))
  loc <- with(grid_bounds,tidyr::crossing(x=seq(xmin,xmax,length.out=flow_dim[1]),
                                                    y=seq(ymin,ymax,length.out=flow_dim[2])))
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
#' @export
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
#' get_capture_zone(wells,aquifer, wIDs = c(1,6))
#' ggplot() + geom_point(data=wells,aes(x,y,color=wID))
get_capture_zone <- function(wells, aquifer, wIDs, t_max, n_particles = 8, type="all") {
  theta <- seq(0,2*pi,2*pi/n_particles)
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
    particle <- track_particle(loc, wells, aquifer, t_max, reverse=TRUE) %>%
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

  ggplot() +
    geom_path(data=particle_path_df,aes(x,y,color=as.factor(wID),group=interaction(pID,wID))) +
    geom_point(data=wells %>% filter(wID==orig_wID,wID %in% c(1,6)),aes(x,y,color=as.factor(wID)),size=3) +
    coord_equal()

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
