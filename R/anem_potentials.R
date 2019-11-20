# anema_potentials.R


#' Estimate the radius of influence of a well
#'
#' @param ... Variable parameters, depending on the method used
#' @param method String containing the name of the desired method
#' @return A numeric value indicating the horizontal radius of influence of the
#'   well.
#' @section Methods: The methods below are taken from Fileccia (2015). Acque
#'   Sotterranee - Italian Journal of Groundwater. DOI 10.7343/AS-117-15-0144.
#'   The following strings can be input for the \code{method} variable, and must be accompanied
#'   by the corresponding variables as part of the \code{...} input:
#'
#'   \describe{
#'     \item{"cooper-jacob":}{\eqn{R=\sqrt{2.25 Tr t / S}}, for confined aquifer after short pumping period. (Cooper and Jacob, 1946)}
#'     \item{"aravin-numerov":}{\eqn{R=\sqrt{1.9 Ksat h t / n}}, for unconfined aquifers (Aravin and Numerov, 1953)}
#'     \item{"sichardt":}{\eqn{R=3000 s \sqrt{Ksat}}, Sichardt formula for unconfined aquifers (Cashman and Preene, 2001)}
#'   }
#'
#'   Where:
#'   \itemize{
#'     \item R = radius of influence [m]
#'     \item Tr = transmissivity [m^2/s]
#'     \item t = time [s]
#'     \item S = storage
#'     \item h = height of the water table above substratum [m]
#'     \item n = effective porosity
#'     \item Ksat = saturated hydraulic conductivity [m/s]
#'     \item s = drawdown in the borehole [m]
#'   }
#' @examples
#' get_ROI(Tr=0.01,t=3600*12,S=1,method="cooper-jacob")
#' get_ROI(Ksat=0.0001,h=50,t=3600*12,n=0.4,method="aravin-numerov")
#' get_ROI(Ksat=0.0001,s=10,method="sichardt")
get_ROI <- function(..., method) {
  vars <- list(...)
  if (method=="cooper-jacob") {
    R <- sqrt(2.25 * vars$Tr * vars$t / vars$S)
  } else if (method=="aravin-numerov") {
    R <- sqrt(1.9 * vars$Ksat * vars$h * vars$t / vars$n)
  } else if (method=="sichardt") {
    R <- 3000 * vars$s * sqrt(vars$Ksat)
  } else {
    stop("Need to properly specify the \"method\" argument")
  }
  return(R)
}

#' Get row of a data.frame as a vector
#'
#' return the row of a data.frame or tibble as a vector (used for extracting x,y coordinates)
get_row_as_vector <- function(df,row=1) {
  return(df %>% dplyr::slice(row) %>% unlist(.,use.names = FALSE))
}

#' Get cumulative effect of wells at location
#'
#' Get the hydraulic head at a location, accounting for the aquifer type
#' (confined or unconfined), resting head, and cumulative effect of all wells. This function
#' is a wrapper around \code{get_potential_differential()} to get the actual hydraulic head instead of the differential potential.
#'
#' @param loc coordinates vector as c(x,y), with units of [m] or as data.frame with columns $x and $y
#' @param wells wells object with each row containing rate Q [m^3/s], diam [m],
#'   radius of influence R [m], & coordinates x [m], y [m]
#' @param h0 resting hydraulic head [m] of the aquifer without any pumping. For
#'   an unconfined aquifer, this must be the thickness of the water table.
#' @param Ksat saturated hydraulic conductivity [m/s]
#' @param z0 thickness of the confined aquifer (confining case only)
#' @param aquifer_type "confined" or "unconfined". Determines the calculation
#'   and output units.
#' @return The output is the hydraulic head \code{loc}, accounting for the
#'   cumulative effect of all \code{wells} (dP), which is given as hydraulic
#'   head [units=m] if \code{aquifer_type="confined"} or discharge potential
#'   [m^2] if \code{aquifer_type="unconfined"}. Then the head at \code{loc} is:
#'
#'   \describe{
#'   \item{aquifer_type="confined"}{\eqn{h=h_0+dP}}
#'   \item{aquifer_type="unconfined"}{\eqn{h=\sqrt{h_0^2+dP}}} }
#' @examples
#' well1 <- define_wells(x=0,y=0,Q=1e-3,diam=0.75,R=300)
#' well2 <- define_wells(x=0.5,y=0.25,Q=-2e-3,diam=0.8,R=300)
#' aquifer <- define_aquifer(aquifer_type="confined",Ksat=0.00001,h0=0,z0=30)
#'
#'
#' get_hydraulic_head(well1,loc=c(5,5),aquifer)
#' get_hydraulic_head(well2,loc=c(5,5),aquifer)
#' wells <- rbind(well1,well2)
#' get_hydraulic_head(wells,loc=c(5,5),h0=0,Ksat=0.00001,z0=30,aquifer_type="confined")
#'
#' get_hydraulic_head(wells,loc=c(5,5),h0=0,Ksat=0.00001,z0=30,aquifer_type="confined")
#' get_hydraulic_head(wells,loc=c(5,5),h0=30,Ksat=0.00001,aquifer_type="unconfined")
#'
#' grid_pts <- expand.grid(x=seq(0,10,by=5),y=seq(0,10,by=5))
#' get_hydraulic_head(well1,loc=grid_pts,h0=30,Ksat=0.00001,aquifer_type="unconfined")
#' get_hydraulic_head(well2,loc=grid_pts,h0=30,Ksat=0.00001,aquifer_type="unconfined")
#' get_hydraulic_head(wells,loc=grid_pts,h0=30,Ksat=0.00001,aquifer_type="unconfined")
#'
#' # Ensure potentials are even when pumping is symmetric (and opposite sign)
#' well1 <- define_wells(x=0,y=0,Q=1e-3,diam=0.5,R=300)
#' well2 <- define_wells(x=1,y=1,Q=-2e-3,diam=0.5,R=300)
#' well3 <- define_wells(x=0,y=0.5,Q=2e-3,diam=0.5,R=300)
#' well4 <- define_wells(x=0.5,y=1,Q=-2e-3,diam=0.5,R=300)
#' wells_even <- rbind(well1,well1,well2,well3,well4)
#' grid_pts_even <- data.frame(x=c(0,0.5,1),y=c(1,0.5,0))
#' get_hydraulic_head(wells_even,loc=grid_pts_even,h0=40,Ksat=0.00005,aquifer_type="unconfined")
get_hydraulic_head <- function(loc,wells,aquifer) { #h0,Ksat,z0=NA,aquifer_type) {

  dP <- get_potential_differential(loc,wells,aquifer)

  # calculate head using h0 and change in potential
  if (aquifer$aquifer_type=="confined") {
    h <- aquifer$h0 + dP
  } else if (aquifer$aquifer_type=="unconfined") {
    h_squared <- pmax(aquifer$h0^2 + dP,0)
    h <- sqrt(h_squared)
  } else {
    stop("aquifer_type specified as:",aquifer$aquifer_type,". It should be specified as \"confined\" or \"unconfined\".\n")
  }

  # return head
  return(h)
}

#' Numerically calculate flow direction
#'
#' Calculates the numerical derivative of hydraulic head in x and y directions, returning \eqn{-dh/dx}
#'   and \eqn{-dh/dy}.
#'
#' @inheritParams get_hydraulic_head
#' @param show_progress Boolean input parameter. If true and there are >20 combinations of
#' wells and locations, then a progress bar will be printed.
#' @param eps Threshold satisfying numeric derivative
#' @return Outputs the flow direction in the x and y directions. If the input \code{loc} is
#'   a numeric \code{c(x,y)}, then the output is in the same format. If the input is a data.frame,
#'   then the output is also a dataframe with columns \code{dx} and \code{dy}. The two values
#'   indicate the flow direction, and are equivalent to \eqn{-dh/dx}
#'   and \eqn{-dh/dy}.
#' @examples
#' wells <- define_wells(x=c(0,0.5),y=c(0,0.25),Q=c(1e-3,-2e-3),diam=c(0.75,0.8),R=c(300,300))
#' aquifer <- define_aquifer(h0=0,Ksat=0.00001,z0=30,aquifer_type="confined")
#' get_flowdir(loc=c(5,5),wells,aquifer)
#' get_flowdir(wells,loc=c(5,5),h0=30,Ksat=0.00001,aquifer_type="unconfined")
#'
#' grid_pts <- expand.grid(x=seq(0,10,by=5),y=seq(0,10,by=5))
#' get_flowdir(wells,loc=grid_pts,h0=30,Ksat=0.00001,aquifer_type="unconfined")
#'
#' # Injection and pumping well along diagonal line
#' wells2 <- data.frame(x=c(-10,10),y=c(-10,10),Q=c(1e-3,-1e-3),diam=c(0.1,0.1),R=c(300,300))
#' grid_pts2 <- data.frame(x=c(-11,0,11),y=c(-11,0,11))
#' fd2 <- get_flowdir(wells2,loc=grid_pts2,h0=30,Ksat=0.00001,aquifer_type="unconfined")
#'
#' # Two pumping wells along diagonal line
#' wells3 <- data.frame(x=c(-10,10),y=c(-10,10),Q=c(-1e-3,-1e-3),diam=c(0.1,0.1),R=c(300,300))
#' grid_pts3 <- data.frame(x=c(-3,-3,0,3,3,-2,2,-1,1),y=c(-3,3,0,-3,3,-1,1,-2,2))
#' fd3 <- get_flowdir(wells3,loc=grid_pts3,h0=30,Ksat=0.00001,aquifer_type="unconfined")
#'
#' ## plot the flow directions
#' # scale dx and dy for visualization
#' fd3_grid <- grid_pts3 %>% cbind(fd3) %>%
#'    dplyr::mutate(dx_norm=dx*50,dy_norm=dy*50,x2=x+dx_norm,y2=y+dy_norm) # normaliz
#' # alternatively, apply nonlinear scaling to dx and dy for visualization
#' fd3_grid <- grid_pts3 %>% cbind(fd3) %>%
#'   dplyr::mutate(angle=atan(dy/dx),mag=sqrt(dx^2+dy^2),mag_norm=mag^(1/2)*5,
#'                 dx_norm=mag_norm*cos(angle)*sign(dx),dy_norm=mag_norm*sin(angle)*sign(dx),
#'                 x2=x+dx_norm,y2=y+dy_norm)
#'
#' library(ggplot2)
#' ggplot(fd3_grid,aes(x,y)) + geom_point(size=2,shape=1) + geom_segment(aes(xend=x2,yend=y2),arrow=arrow(type="closed",length=unit(2,"mm"))) + coord_equal()
get_flowdir <- function(loc,wells,aquifer,show_progress=FALSE,eps=1e-4) {

  loc_class <- class(loc)
  # get change in potential due to wells
  if (identical(loc_class,"numeric") | identical(loc_class,"integer")) { # loc is a vector as c(x, y)
    fd <- -numDeriv::grad(get_hydraulic_head,loc,wells=wells,aquifer=aquifer)
  } else if (max(grepl("data.frame",class(loc)))) { # loc is a data.frame with $x and $y columns
    fd <- data.frame(dx=NULL,dy=NULL)
    loc_list <- lapply(split(loc %>% dplyr::select(x,y),1:dim(loc)[1]),get_row_as_vector)
    n <- length(loc_list)

    if (n * dim(wells)[1] < 20 | !show_progress) { # no progress bar
      for (i in 1:n) {
        fd_i <- -numDeriv::grad(get_hydraulic_head,loc_list[[i]],method.args=list(eps=eps),wells=wells,aquifer=aquifer)
        fd_i_df <- data.frame(dx=fd_i[1],dy=fd_i[2])
        fd <- rbind(fd,fd_i_df)
      }
    } else { # with progress bar
      start_time <- Sys.time()
      cat(paste0("\nGetting flow direction at each point (",dim(loc)[1]," points, ",dim(wells)[1]," wells):\n"))
      pb <- txtProgressBar(min = 1, max = n, initial = 1, char = "=",width = NA, style = 3)
      for (i in 1:n) {
        fd_i <- -numDeriv::grad(get_hydraulic_head,loc_list[[i]],wells=wells,aquifer=aquifer)
        fd_i_df <- data.frame(dx=fd_i[1],dy=fd_i[2])
        fd <- rbind(fd,fd_i_df)
        setTxtProgressBar(pb, i)
      }
      cat("\n")
    }
  }

  return(fd)
}


#' Get cumulative effect of wells at location
#'
#' Get the cumulative effect of all wells at a singled location, output as head (confined aquifer) or discharge potential (unconfined aquifer).
#'
#' @param loc coordinates \code{data.frame} with columns labeled 'x' and 'y', or as vector as c(x,y), with units of [m]
#' @inheritParams get_hydraulic_head
#' @return The output is the cumulative effect at \code{loc} of all \code{wells} on the hydraulic head [units=m] if
#'   \code{aquifer_type="confined"} or discharge potential [m^2] if
#'   \code{aquifer_type="unconfined"}.
#'
#'   Note: if the \code{loc} is contained within the diameter of a well, the distance between the location
#'   and that well is automatically adjusted to the edge of the well screen (i.e., well$diam/2). Similar any well-location
#'   distance that exceeds the radius of influence of the well, R, is set equal to R
#' @examples
#' # Single test location
#' wells <- define_wells(x=c(0,0.5),y=c(0,0.25),Q=c(1e-4,-2e-4),diam=c(0.75,0.8),R=c(300,300))
#' aquifer <- define_aquifer(aquifer_type="confined",Ksat=0.00001,h0=0,z0=30)
#'
#' wells <- rbind(well1,well2)
#' get_potential_differential(loc=c(50,50),well1,aquifer)
#' get_potential_differential(loc=c(50,50),well2,aquifer)
#'
#' # Multiple test locations
#' wells <- data.frame(x=c(-10,10),y=c(-10,10),Q=c(1e-3,-1e-3),diam=c(0.1,0.1),R=c(300,300))
#' grid_pts <- data.frame(x=c(-11,0,11),y=c(-11,0,11))
#' get_potential_differential(grid_pts,wells,Ksat=0.00001,aquifer_type="unconfined")
get_potential_differential <- function(loc, wells, aquifer) {
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

  Ri <- matrix(rep(R,each=mj),ncol=ni)
  Qi <- matrix(rep(Q,each=mj),ncol=ni)
  di <- matrix(rep(diam_wells,each=mj),ncol=ni)

  # rji is the distance between well i and observation location j, with 2 exceptions
  # 1. set rji to the radius of influence, R, for any well-location distance that exceeds R
  # 2. set rji to the radius of the well, diam/2, for anly location that falls within the radius of the well, diam/2
  rji <- pmax(pmin(sqrt((xi-xj)^2 + (yi-yj)^2), Ri), di/2)

  # calculate the potential differential
  if (aquifer$aquifer_type=="confined") { # as a differential hydraulic head
    if(is.na(aquifer$z0)) {
      stop("need to set z0 for confined aquifer")
    }
    dP <- -rowSums(Qi/(2*pi*aquifer$Ksat * aquifer$z0)*log(rji/Ri))

  } else if (aquifer$aquifer_type =="unconfined") { # as a differential discharge potential
    dP <- -rowSums(Qi/(pi*aquifer$Ksat)*log(rji/Ri))
  }

  return(dP)
}




