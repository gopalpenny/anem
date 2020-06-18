# old_anem_particle_tracking.R


#' Get flowlines in confined aquifer
#'
#' Get flowlines in confined aquifer
#' @param wells Wells object
#' @param aquifer Aquifer object
#' @param flow_dim Dimensions for generating raster for generating flowlines
#' @keywords internal
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' # define aquifer
#' bounds_df <- data.frame(bound_type=c("NF","NF","NF","NF"),m=c(Inf,0,Inf,0),b=c(0,1000,1000,0))
#' aquifer <- define_aquifer("confined",1e-3,bounds=bounds_df,h0=100)
#'
#' # define wells and well images
#' wells_df <- data.frame(x=c(300,700),y=c(200,600),diam=1) %>%
#'   mutate(R=1000,Q=-1/dplyr::n())
#' wells <- define_wells(wells_df) %>% generate_image_wells(aquifer)
#'
#' library(ggplot2)
#' ggplot() +
#'   geom_segment(data=aquifer_unconfined$bounds,aes(x1,y1,xend=x2,yend=y2,color=bound_type)) +
#'   geom_point(data=wells %>% filter(wID==orig_wID),aes(x,y),shape=21) +
#'   geom_path(data=cl0,aes(x,y,color=as.factor(line))) +
#'   coord_equal()
#' }
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


#' Get capture zone regions
#'
#' Get well capture zone regions
#' @param type specifies which results to return. One of \code{"all"}, \code{"paths"}, \code{"endpoints"}, or \code{"smoothed"}. See details.
#' @keywords internal
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
#' @keywords internal
#' @examples
#' \dontrun{
#' get_theta(-1,0)
#' get_theta(1,0)
#' get_theta(-1,1)/pi
#' get_theta(0,1)/pi
#' get_theta(c(1,1,0,-1,-1),c(0,1,1,1,0))/pi
#' }
get_theta <- function(x, y) {
  theta <- atan(y/x) + pi*(x<0)*dplyr::case_when(
    y>=0~1,
    y<0~-1,
    TRUE~0)
  return(theta)
}
