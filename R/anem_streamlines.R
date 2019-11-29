# stream_function.R

#' Get stream function
#'
#' Get stream function at given locations for a set of wells in a confined aquifer.
#' Taken from Strack, 2017 Chapter 2: eqn 2.56
#' @inheritParams get_potential_differential
#' @return Returns a vector containing streamfunction values corresponding to the
#'   (x, y) points in \code{loc}. The streamfunction associated with each well is
#'   \eqn{\Psi= Q/(2 \pi) \theta}, with \eqn{\theta} from \eqn{[-\pi,\pi]}. This means
#'   The streamfunction will have multiple values for \eqn{\theta=\pi=-\pi} for each well. The
#'   flow net at any location is then the sum of the stream functions for all wells.
#' @export
#' @examples
#' # Create a grid of locations
#' loc <- crossing(x=seq(-200,200,length.out=201),y=seq(-200,200,length.out=201))
#'
#' # Constant head boundary
#' wells_constant_head <- define_wells(x=c(-100,100),y=c(-0,0),Q=c(1e-2,-1e-2),diam=c(0.1,0.1),R=c(500,500))
#' aquifer <- define_aquifer("confined",1e-4,z0=20,h0=0)
#' constant_head_boundary <- loc %>%
#'   dplyr::bind_cols(streamfunction=get_stream_function(loc,wells_constant_head,aquifer)) %>%
#'   dplyr::bind_cols(head=get_hydraulic_head(loc,wells_constant_head,aquifer))
#' ggplot() +
#'   geom_contour(data=constant_head_boundary,aes(x,y,z=head),bins=20,linetype="dashed") +
#'   geom_contour(data=constant_head_boundary,aes(x,y,z=streamfunction),bins=20) +
#'   geom_point(data=wells_constant_head,aes(x,y,fill=well_type),size=3,shape=21) +
#'   theme(legend.position=c(0.8,0.1),legend.title=element_blank()) +
#'   coord_equal()
#'
#' # No flow boundary
#' wells_no_flow <- define_wells(x=c(-100,100),y=c(-0,0),Q=c(-1e-2,-1e-2),diam=c(0.1,0.1),R=c(500,500))
#' no_flow_boundary <- loc %>%
#'   dplyr::bind_cols(streamfunction=get_stream_function(loc,wells_no_flow,aquifer)) %>%
#'   dplyr::bind_cols(head=get_hydraulic_head(loc,wells_no_flow,aquifer))
#' ggplot() +
#'   geom_contour(data=no_flow_boundary,aes(x,y,z=head),bins=20,linetype="dashed") +
#'   geom_contour(data=no_flow_boundary,aes(x,y,z=streamfunction),bins=50) +
#'   geom_point(data=wells_no_flow,aes(x,y,fill=well_type),size=3,shape=21) +
#'   theme(legend.position=c(0.8,0.1),legend.title=element_blank()) +
#'   coord_equal()
get_stream_function <- function(loc, wells, aquifer) {
  x_well <- wells$x
  y_well <- wells$y
  R <- wells$R
  Q <- wells$Q
  diam_wells <- wells$diam

  if (max(grepl("data.frame",class(loc)))) {
    x_loc <- loc$x
    y_loc <- loc$y
  } else {
    x_loc <- loc[1]
    y_loc <- loc[2]
  }

  ni <- length(x_well) # number of wells
  mj <- length(x_loc) # number of locations to measure potential

  # create mj x ni matrices -- rows j vary for locations, columns i for wells
  xi <- matrix(rep(x_well,each=mj,nrow=ni), ncol=ni) # mj x ni matrix
  yi <- matrix(rep(y_well,each=mj,nrow=ni), ncol=ni) # mj x ni matrix

  xj <- matrix(rep(x_loc,ni,nrow=ni), ncol=ni)
  yj <- matrix(rep(y_loc,ni,nrow=ni), ncol=ni)

  Qi <- matrix(rep(Q,each=mj),ncol=ni)
  theta_i=atan((yj-yi)/(xj-xi))+(xj<xi)*sign((yj>=yi)-0.5)*pi
  di <- matrix(rep(diam_wells,each=mj),ncol=ni)

  # Check to make sure r < R (none of the wells reaches it's full extent)
  Ri <- matrix(rep(R,each=mj),ncol=ni)
  rji <- sqrt((xi-xj)^2 + (yi-yj)^2)
  if (max(rji > Ri)) {
    warning("get_stream_function does not account for radius of influence.")
  }

  if (FALSE) {
    # debugging
    gr <- data.frame(x=xj,y=yj,theta=theta_i)
    ggplot() + geom_raster(data=gr,aes(x,y,fill=theta_i))
  }

  # rji is the distance between well i and observation location j, with 2 exceptions
  # 1. set rji to the radius of influence, R, for any well-location distance that exceeds R
  # 2. set rji to the radius of the well, diam/2, for anly location that falls within the radius of the well, diam/2
  rji <- pmax(pmin(sqrt((xi-xj)^2 + (yi-yj)^2), Ri), di/2)

  # calculate the stream function
  if (aquifer$aquifer_type=="confined") { #
    stream_function <- rowSums(Qi/(2*pi)*theta_i)

  } else if (aquifer$aquifer_type =="unconfined") { #
    stop("Get_stream_function() can only get stream functions for confined aquifers.")
  }

  return(stream_function)
}

