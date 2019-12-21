# anem_particle_tracking.R

#' Particle velocity
particle_velocity_m_day <- function(t, loc, params) {
  loc <- get_flowdir(loc,params$wells,params$aquifer) * params$aquifer$Ksat / params$n * 3600 * 24
  return(list(loc))
}

#' Track particle in aquifer
#'
#' Track a particle in the aquifer from an original location to a pumping well,
#' aquifer boundary, or outside of the wells ROI
#' @param loc Coordinate vector as \code{c(x, y)}
#' @param wells Wells \code{data.frame} containing well images
#' @param aquifer Aquifer as an \code{aquifer} object, with \code{Ksat} and porosity, \code{n}
#' Returns a data.frame containing the time and locations
#' @examples
#' bounds_df <- data.frame(bound_type=c("NF","NF","CH","NF"),m=c(Inf,0,Inf,0),b=c(0,1000,1000,0))
#' aquifer <- define_aquifer(aquifer_type="confined",Ksat=0.001,n=0.4,h0=0,z0=20,bounds=bounds_df)
#' wells <- data.frame(x=c(500,100),y=c(300,600),Q=c(-1e-1,-1e-1),diam=c(1,1),R=c(1000,100)) %>%
#'   define_wells() %>% generate_image_wells(aquifer)
#' gridded <- get_gridded_hydrodynamics(wells,aquifer,c(100,100),c(10,10))
#' system.time(particle_path <- track_particle(c(750,200), wells, aquifer))
#' particle_path <- track_particle(c(500,250), wells, aquifer)
#' ggplot() +
#'   geom_raster(data=gridded$head,aes(x,y,fill=head_m)) +
#'   geom_segment(data=aquifer$bounds,aes(x1,y1,xend=x2,yend=y2,color=bound_type)) +
#'   geom_point(data=wells %>% filter(wID==orig_wID),aes(x,y),shape=21) +
#'   geom_path(data=particle_path,aes(x,y),color="red") +
#'   coord_equal()
track_particle <- function(loc, wells, aquifer) {

  # function for roots (ie, when to stop numerical integration)
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
  params <- list(wells=wells_no_diam, orig_wells=orig_wells, aquifer=aquifer, n=aquifer$n)

  d_bounds <- get_distance_to_bounds(x, params$aquifer$bounds)
  d_wells <- sqrt((params$orig_wells$x - loc[1])^2 + (params$orig_wells$y - loc[2])^2)

  particle <- cbind(time=0,x=loc[1],y=loc[2])
  last <- particle[dim(particle)[1],]
  i <- 1
  while (all(d_wells > params$orig_wells$diam) & d_bounds > 1 & i < 50) {
    # print(i)

    # get new travel time guess
    v <- particle_velocity_m_day(0, loc, params)[[1]]
    min_dist <- min(d_wells,d_bounds)
    travel_time_guess <- min_dist / sqrt(v[1]^2 + v[2]^2)

    # get particle tracking based on first guess of travel time
    ########################################################
    ########################################################
    # ptm <- proc.time()
    start_loc <- as.numeric(last[c("x","y")])
    new_times <- seq(last["time"],last["time"] + travel_time_guess,length.out = 50)
    new_particle <- radau(start_loc, new_times, particle_velocity_m_day,
                          parms=params, rootfunc = rootfun)
    # proc.time() - ptm
    ########################################################
    ########################################################

    particle <- rbind(particle,new_particle[-1,])
    last <- particle[dim(particle)[1],]

    # get distance to objects
    d_bounds <- get_distance_to_bounds(x, params$aquifer$bounds)
    d_wells <- sqrt((params$orig_wells$x - last["x"])^2 + (params$orig_wells$y - last["y"])^2)

    i <- i + 1
  }

  particle_path <- particle %>% tibble::as_tibble() %>% setNames(c("time","x","y"))
  return(particle_path)
}


#' Get flowlines in confined aquifer
#'
#' Get flowlines in confined aquifer
#' @param wells
#' @param aquifer
#' @param flow_dim
#' @examples
#' # define aquifer
#' bounds_df <- data.frame(bound_type=c("NF","NF","NF","NF"),m=c(Inf,0,Inf,0),b=c(0,1000,1000,0))
#' aquifer <- define_aquifer("confined",1e-3,bounds=bounds_df,h0=100)
#'
#' # define wells and well images
#' wells_df <- data.frame(x=c(300,700),y=c(200,600),diam=1) %>%
#'   mutate(R=1000,Q=-1/n())
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
      geom_point(data=a %>% filter(d2_flag),aes(x,y,color=d2_change))
    ggplot(a) + geom_path(aes(i,d2_change,color=as.factor(line)))
    l_axis <- cl0 %>% dplyr::filter(abs(y-well_i$y) < y_diff,x < well_i$x)
  }
  cl <- cl0 %>% mutate(dist=sapply(y,function(a) min(abs(a-wells_constant_head$y)))) # %>% filter(dist>y_diff*2)
  cl <- cl %>% group_by(line,level) %>% mutate(min_dist = min(dist),max_y=max(y)) %>% filter(min_dist < 2, max_y > 150)

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