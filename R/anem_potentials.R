# anema_potentials.R


#' Estimate the radius of influence of a well
#'
#' @param ... Variable parameters, depending on the method used. Single numbers or vectors (of equal length)
#' @param method String containing the name of the desired method
#' @return A numeric value indicating the horizontal radius of influence of the
#'   well.
#' @section Methods: The methods below are taken from Fileccia (2015). Acque
#'   Sotterranee - Italian Journal of Groundwater
#'   (\url{http://www.doi.org/10.7343/AS-117-15-0144}).
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
#' These inputs can be single numbers or vectors of equal length.
#' @export
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

#' Get hydraulic head
#'
#' Get hydraulic head at location, accounting for the cumulative effect of wells at location, the aquifer type
#' (confined or unconfined), resting head, and cumulative effect of all wells. This function
#' is a wrapper around \code{get_potential_differential()} to get the actual hydraulic head instead of the differential potential.
#'
#' @param loc coordinates vector as c(x,y), with units of [m] or as data.frame with columns $x and $y
#' @param wells wells object with each row containing rate Q [m^3/s], diam [m],
#'   radius of influence R [m], & coordinates x [m], y [m]
#' @param aquifer Afuifer object containing aquifer_type, h0, Ksat, bounds, z0 (for confined case only)
#' @return The output is the hydraulic head \code{loc}, accounting for the
#'   cumulative effect of all \code{wells} (dP), which is given as hydraulic
#'   head [units=m] if \code{aquifer_type="confined"} or discharge potential
#'   [m^2] if \code{aquifer_type="unconfined"}. Then the head at \code{loc} is:
#'
#'   \describe{
#'   \item{aquifer_type="confined"}{\eqn{h=h_0+dP}}
#'   \item{aquifer_type="unconfined"}{\eqn{h=\sqrt{h_0^2+dP}}} }
#' @export
#' @examples
#' well1 <- define_wells(x=0,y=0,Q=1e-3,diam=0.75,R=300)
#' well2 <- define_wells(x=0.5,y=0.25,Q=-2e-3,diam=0.8,R=300)
#' aquifer <- define_aquifer(aquifer_type="confined",Ksat=0.00001,h0=0,z0=30)
#'
#'
#' get_hydraulic_head(well1,loc=c(5,5),aquifer)
#' get_hydraulic_head(well2,loc=c(5,5),aquifer)
#' wells <- rbind(well1,well2)
#'
#' get_hydraulic_head(wells,loc=c(5,5),aquifer=aquifer)
#'
#' grid_pts <- expand.grid(x=seq(0,10,by=5),y=seq(0,10,by=5))
#' aquifer_unconfined <- define_aquifer(aquifer_type="confined",Ksat=0.00001,h0=50,z0=30)
#' get_hydraulic_head(well1,loc=grid_pts,aquifer=aquifer_unconfined)
#' get_hydraulic_head(well2,loc=grid_pts,aquifer=aquifer_unconfined)
#' get_hydraulic_head(wells,loc=grid_pts,aquifer=aquifer_unconfined)
#' get_hydraulic_head(NULL,loc=grid_pts,aquifer=aquifer_unconfined)
#'
#' # Ensure potentials are even when pumping is symmetric (and opposite sign)
#' well1 <- define_wells(x=0,y=0,Q=1e-3,diam=0.5,R=300)
#' well2 <- define_wells(x=1,y=1,Q=-2e-3,diam=0.5,R=300)
#' well3 <- define_wells(x=0,y=0.5,Q=2e-3,diam=0.5,R=300)
#' well4 <- define_wells(x=0.5,y=1,Q=-2e-3,diam=0.5,R=300)
#' wells_even <- rbind(well1,well1,well2,well3,well4)
#' grid_pts_even <- data.frame(x=c(0,0.5,1),y=c(1,0.5,0))
#' get_hydraulic_head(wells_even,loc=grid_pts_even,aquifer=aquifer_unconfined)
get_hydraulic_head <- function(loc,wells,aquifer) { #h0,Ksat,z0=NA,aquifer_type) {

  dP <- get_potential_differential(loc,wells,aquifer)

  if (is.null(aquifer$recharge)) {
    undisturbed_potential <- ifelse(aquifer$aquifer_type=="confined",aquifer$h0,aquifer$h0^2)
  } else {
    undisturbed_potential <- get_recharge_undisturbed_potential(loc,aquifer)
  }

  # calculate head using h0 and change in potential
  if (aquifer$aquifer_type=="confined") {
    h <- undisturbed_potential + dP
  } else if (aquifer$aquifer_type=="unconfined") {
    h_squared <- pmax(undisturbed_potential + dP,0)
    h <- sqrt(h_squared)
  } else {
    stop("aquifer_type specified as:",aquifer$aquifer_type,". It should be specified as \"confined\" or \"unconfined\".\n")
  }

  # return head
  return(h)
}

#' Calculate the flow direction
#'
#' Calculates the derivative of hydraulic head in x and y directions, returning \eqn{-dh/dx}
#'   and \eqn{-dh/dy}. For confined aquifers, the result is calculated using the sum of the
#'   effect of each analytical element, whereas for unconfined aquifers the result can be calculated
#'   analytically or numerically from hydraulic head.
#'
#' @inheritParams get_hydraulic_head
#' @param show_progress Boolean input parameter. If true and there are >20 combinations of
#' wells and locations, then a progress bar will be printed.
#' @param eps Threshold satisfying numeric derivative
#' @param numderiv Boolean that determines whether unconfined aquifers are calculated analytically or numerically
#' @return Outputs the flow direction in the x and y directions. If the input \code{loc} is
#'   a numeric \code{c(x,y)}, then the output is in the same format. If the input is a data.frame,
#'   then the output is also a data.frame with columns \code{dx} and \code{dy}. The two values
#'   indicate the flow direction, and are equivalent to \eqn{-dh/dx}
#'   and \eqn{-dh/dy}.
#' @export
#' @examples
#' wells <- define_wells(x=c(0,0.5),y=c(0,0.25),Q=c(1e-3,-2e-3),diam=c(0.75,0.8),R=c(300,300))
#' aquifer <- define_aquifer(h0=0,Ksat=0.00001,z0=30,aquifer_type="confined")
#' get_flowdir(loc=c(5,5),wells,aquifer)
#'
#' grid_pts <- expand.grid(x=seq(0,10,by=5),y=seq(0,10,by=5))
#' get_flowdir(loc=grid_pts,wells,aquifer)
#'
#' # Injection and pumping well along diagonal line
#' wells2 <- data.frame(x=c(-10,10),y=c(-10,10),Q=c(1e-3,-1e-3),diam=c(0.1,0.1),R=c(300,300))
#' grid_pts2 <- data.frame(x=c(-11,0,11),y=c(-11,0,11))
#' aquifer_unconfined <- define_aquifer(aquifer_type="unconfined",Ksat=0.00001,h0=20)
#' fd2_a <- get_flowdir(loc=grid_pts2,wells2,aquifer_unconfined)
#' fd2_b <- get_flowdir(loc=grid_pts2,wells2,aquifer_unconfined,numderiv=TRUE)
#' fd2_a - fd2_b
#'
#' # Two pumping wells along diagonal line
#' wells3 <- data.frame(x=c(-10,10),y=c(-10,10),Q=c(-1e-3,-1e-3),diam=c(0.1,0.1),R=c(300,300))
#' grid_pts3 <- data.frame(x=c(-3,-3,0,3,3,-2,2,-1,1),y=c(-3,3,0,-3,3,-1,1,-2,2))
#' fd3 <- get_flowdir(wells3,loc=grid_pts3,aquifer_unconfined)
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
#' ggplot(fd3_grid,aes(x,y)) + geom_point(size=2,shape=1) +
#'   geom_segment(aes(xend=x2,yend=y2),arrow=arrow(type="closed",length=unit(2,"mm"))) + coord_equal()
#'
#'
#' wells <- define_wells(Q=0.1,x=-2,y=-2,R=100,diam=0.5)
#' recharge_params <- list(recharge_type="F",recharge_vector=c(0,0,-1,-1),flow=1e-3,x0=0,y0=0)
#' aquifer <- define_aquifer("confined",1e-3,h0=0,z0=1,recharge=recharge_params)
#' get_flowdir(c(-1,-1),wells,aquifer)
#' get_flowdir(c(-1,-1),NULL,aquifer)
#'
#' recharge_params <- list(recharge_type="D",recharge_vector=c(0,0,-1,-1),flow_main=1,flow_opp=1,x0=0,y0=0)
#' aquifer <- define_aquifer("confined",1,h0=0,z0=1,recharge=recharge_params)
#' loc <- expand.grid(x=-1:1,y=-1:1)
#' loc %>% bind_cols(get_flowdir(loc,wells,aquifer))
#' loc %>% bind_cols(get_flowdir(loc,NULL,aquifer))
#'
#' recharge_params <- list(recharge_type="D",recharge_vector=c(10,10,11,11),flow_main=sqrt(2),flow_opp=sqrt(2),x0=0,y0=0)
#' aquifer <- define_aquifer("confined",1,h0=0,z0=1,recharge=recharge_params)
#' loc <- expand.grid(x=9:11,y=9:11)
#' get_flowdir(loc,wells,aquifer)
#' get_flowdir(loc,wells,aquifer)
get_flowdir <- function(loc,wells,aquifer,show_progress=FALSE,eps=1e-6,numderiv=FALSE) {
  cAquifer <- check_aquifer(aquifer)

  loc_class <- class(loc)
  # get change in potential due to wells
  if (aquifer$aquifer_type == "confined") {
    fd <- get_flowdir_raw(loc,wells,aquifer) / (2 * aquifer$z0)

  } else if (aquifer$aquifer_type == "unconfined" & !numderiv) {
    h <- get_hydraulic_head(loc,wells,aquifer)
    if (any(h==0)) {
      warning("In some locations head is equal to 0. Aquifer fully depleted and results unreliable.")
    }

    if (identical(loc_class,"numeric") | identical(loc_class,"integer")) { # loc is a vector as c(x, y)
      fd <- get_flowdir_raw(loc,wells,aquifer) / (2 * h)

    } else if (max(grepl("data.frame",class(loc)))) { # loc is a data.frame with $x and $y columns
      fd <- get_flowdir_raw(loc,wells,aquifer)
      fd$dx <- fd$dx / (2 * h)
      fd$dy <- fd$dy / (2 * h)
    }
  } else if (aquifer$aquifer_type == "unconfined" & numderiv) {
    if (identical(loc_class,"numeric") | identical(loc_class,"integer")) { # loc is a vector as c(x, y)
      fd <- -numDeriv::grad(get_hydraulic_head,loc,wells=wells,aquifer=aquifer)
    } else if (max(grepl("data.frame",class(loc)))) { # loc is a data.frame with $x and $y columns
      fd <- data.frame(dx=NULL,dy=NULL)
      loc_list <- lapply(split(loc %>% dplyr::select(x,y),1:dim(loc)[1]),get_row_as_vector)
      n <- length(loc_list)

      if (!null_or_missing(wells)) {
        n_wells <- dim(wells)[1]
      } else {
        n_wells <- 1
      }
      if (n * n_wells < 20 | !show_progress) { # no progress bar
        for (i in 1:n) {
          fd_i <- -numDeriv::grad(get_hydraulic_head,loc_list[[i]],method="simple",method.args=list(eps=eps),wells=wells,aquifer=aquifer)
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
    # end "unconfined" flow
  } else {
    stop("aquifer_type not specified as confined or unconfined")
  }

  return(fd)
}


#' Get potential differential
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
#' @export
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
#' aquifer_unconfined <- define_aquifer(aquifer_type="confined",Ksat=0.00001,h0=0,z0=30)
#' get_potential_differential(grid_pts,wells,aquifer_unconfined)
#'
#' get_potential_differential(c(1,1))
#' get_potential_differential(data.frame(x=c(1,1),y=c(2,2)))
get_potential_differential <- function(loc, wells, aquifer) {
  if (null_or_missing(wells)) {
    if (max(grepl("data.frame",class(loc)))) {
      return(rep(0,nrow(loc)))
    } else {
      return(0)
    }  }
  x_well <- wells$x
  y_well <- wells$y
  R <- wells$R
  Q <- wells$Q
  diam_wells <- wells$diam

  if (any(grepl("data.frame",class(loc)))) {
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

#' Get flow direction in confined aquifer
#'
#' Get the cumulative effect of all wells on flow at a single location, output as dh/dx and dh/dy.
#'
#' @param loc coordinates \code{data.frame} with columns labeled 'x' and 'y', or as vector as c(x,y), with units of [m]
#' @inheritParams get_hydraulic_head
#' @return The output is the cumulative effect at \code{loc} of all \code{wells} on the flow
#'   Note: if the \code{loc} is contained within the diameter of a well, the distance between the location
#'   and that well is automatically adjusted to the edge of the well screen (i.e., well$diam/2). Similar any well-location
#'   distance that exceeds the radius of influence of the well, R, is set equal to R
#' @examples
#' wells <- define_wells(x=c(0,0.5),y=c(0,0.25),Q=c(1e-3,-2e-3),diam=c(0.75,0.8),R=c(300,300))
#' aquifer <- define_aquifer(h0=0,Ksat=0.00001,z0=30,aquifer_type="confined")
#' get_flowdir(loc=c(5,5),wells,aquifer)
#' get_flowdir_raw(loc=c(5,5),wells,aquifer)
#' get_flowdir_raw(loc=c(5,5),NULL,aquifer)
#'
#' grid_pts <- expand.grid(x=seq(0,10,by=5),y=seq(0,10,by=5))
#' get_flowdir(loc=grid_pts,wells,aquifer)
#' get_flowdir_raw(loc=grid_pts,wells,aquifer)
#' get_flowdir_raw(loc=grid_pts,NULL,aquifer)
#'
#' recharge_params <- list(recharge_type="F",recharge_vector=c(0,0,-1,-1),flow=1e-3,x0=0,y0=0)
#' aquifer_norecharge <- define_aquifer("confined",1e-3,h0=0,z0=1)
#' aquifer_recharge <- define_aquifer("confined",1e-3,h0=0,z0=1,recharge=recharge_params)
#' (a <- get_flowdir(c(-1,-1),wells,aquifer_recharge))
#' (b <- get_flowdir(c(-1,-1),wells,aquifer_norecharge))
#' (d <- get_flowdir(c(-1,-1),NULL,aquifer_recharge))
#' b + d
#'
#' recharge_params <- list(recharge_type="D",recharge_vector=c(0,0,-1,-1),flow_main=1,flow_opp=1,x0=0,y0=0)
#' aquifer_norecharge <- define_aquifer("confined",1,h0=0,z0=1)
#' aquifer_recharge <- define_aquifer("confined",1,h0=0,z0=1,recharge=recharge_params)
#' loc <- expand.grid(x=-1:1,y=-1:1)
#' (a <- get_flowdir(loc,wells,aquifer_recharge))
#' (b <- get_flowdir(loc,wells,aquifer_norecharge))
#' (d <- get_flowdir(loc,NULL,aquifer_recharge))
#' b + d - a
#'
#' recharge_params <- list(recharge_type="D",recharge_vector=c(10,10,11,10),flow_main=1,flow_opp=1,x0=0,y0=0)
#' aquifer_norecharge <- define_aquifer("confined",1,h0=0,z0=1)
#' aquifer_recharge <- define_aquifer("confined",1,h0=0,z0=1,recharge=recharge_params)
#' loc <- expand.grid(x=9:11,y=9:11)
#' (d <- get_flowdir(loc,NULL,aquifer_recharge))
#' cbind(loc,d)
get_flowdir_raw <- function(loc, wells, aquifer) {

  df_input <- any(grepl("data.frame",class(loc)))
  if (df_input) {
    x_loc <- loc$x
    y_loc <- loc$y
  } else {
    x_loc <- loc[1]
    y_loc <- loc[2]
  }

  if (!null_or_missing(wells)) {
    x_well <- wells$x
    y_well <- wells$y
    R <- wells$R
    Q <- wells$Q
    diam_wells <- wells$diam
  } else {
    x_well <- 0
    y_well <- 0
    R <- 100
    Q <- 0
    diam_wells <- 1
  }

  if (aquifer$aquifer_type=="confined") { # check for z0
    if(is.na(aquifer$z0)) {
      stop("need to set z0 for confined aquifer")
    }
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

  if (is.null(aquifer$recharge)) {
    fd_r_x <- rep(0,mj)
    fd_r_y <- rep(0,mj)
  } else {
    rech <- aquifer$recharge
    # 1. Recharge type: Flow
    if (rech$recharge_type == "F") {
      fd_r_x <- rep(rech$x_term,mj)
      fd_r_y <- rep(rech$y_term,mj)
    # 2. Recharge type: Divide
    } else if (rech$recharge_type == "D") {
      if (abs(rech$divide_m)!=Inf) {
        main_side <- sign(y_loc - (rech$divide_m * x_loc + rech$divide_b)) != -rech$main_side_y
      } else {
        main_side <- sign(x_loc - rech$divide_b) != -rech$main_side_x
      }

      fd_r_x <- rep(rech$x_term_main,mj)
      fd_r_x[!main_side] <- rech$x_term_opp
      fd_r_y <- rep(rech$y_term_main,mj)
      fd_r_y[!main_side] <- rech$y_term_opp
    }
    if (aquifer$aquifer_type=="confined") {
      fd_r_x <- fd_r_x * 2 * aquifer$z0
      fd_r_y <- fd_r_y * 2 * aquifer$z0
    } else {
    }
  }

  # rji is the distance between well i and observation location j, with 2 exceptions
  # 1. set rji to Inf for any well-location distance that exceeds R or
  # 2. set rji to Inf for anly location that falls within the radius of the well, diam/2
  rji <- sqrt((xi-xj)^2 + (yi-yj)^2)
  rji[rji <= di/2 | rji > Ri] <- Inf

  # calculate the potential differential
  dx <- rowSums(Qi/(pi*aquifer$Ksat) * (xj - xi)/rji^2)
  dy <- rowSums(Qi/(pi*aquifer$Ksat) * (yj - yi)/rji^2)
  # dx <- rowSums(Qi/(2*pi*aquifer$Ksat * aquifer$z0) * 1/rji^2 * (xj - xi)) ## OLD
  # dy <- rowSums(Qi/(2*pi*aquifer$Ksat * aquifer$z0) * 1/rji^2 * (yj - yi)) ## OLD

  if (df_input) {
    fd <- data.frame(dx=dx - fd_r_x,dy=dy - fd_r_y)
  } else {
    fd <- c(dx - fd_r_x, dy - fd_r_y)
  }

  return(fd)
}

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
  theta_i <- atan((yj-yi)/(xj-xi))+(xj<xi)*sign((yj>=yi)-0.5)*pi
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



