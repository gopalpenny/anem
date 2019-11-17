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
#' get_radius_of_influence(Tr=0.01,t=3600*12,S=1,method="cooper-jacob")
#' get_radius_of_influence(Ksat=0.0001,h=50,t=3600*12,n=0.4,method="aravin-numerov")
#' get_radius_of_influence(Ksat=0.0001,s=10,method="sichardt")
get_radius_of_influence <- function(..., method) {
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


#' Get the effect of a single well
#'
#' Get the effect of a single well on hydraulic potential or discharge
#' potential.
#'
#' @param well single well object containing rate Q [m^3/s], diam [m],
#'   radius of influence roi [m], & coordinates x0 [m], y0 [m]
#' @param loc coordinates vector as c(x,y), with units of [m]
#' @inheritParams get_hydraulic_head
#' @return The output is the effect at \code{loc} of the \code{well} on the hydraulic head (in units of [m]) if
#'   \code{aquifer_type="confined"} or discharge potential [m^2] if
#'   \code{aquifer_type="unconfined"}.
#'
#'   Note: if the \code{loc} is contained within the diameter of the well, the distance between the tested location
#'   and the well is automatically adjusted to the edge of the well screen (i.e., well$diam/2).
#' @examples
#' well <- data.frame(pID=1,x0=0,y0=0,rate=1e-4,diam=0.75,roi=300)
#' get_well_effect(well,loc=c(50,50),Ksat=0.00001,z0=10,aquifer_type="confined")
#' get_well_effect(well,loc=c(50,50),Ksat=0.00001,aquifer_type="unconfined")
get_well_effect <- function(well,loc,Ksat,z0=NULL,aquifer_type) {
  x1 <- loc[1]
  y1 <- loc[2]
  x0 <- well$x0
  y0 <- well$y0
  r <- sqrt((x1-x0)^2+(y1-y0)^2)

  # If the location is inside the well, set the distance at the edge of the well screen
  if (r < well$diam/2) {
    r <- well$diam/2
  }

  if (r > well$roi) {
    dp <- 0
  } else if (aquifer_type=="confined") {
    if(is.na(z0)) {
      stop("For confined aquifer, must specify z0")
    }
    # Transmissivity: as Ksat * z0, the thickness of the aquifer [L^2/T]. Must have same length and time units as Q
    Transmissivity <- Ksat * z0
    dp <- -well$rate/(2*pi*Ksat * z0)*log(r/well$roi)
  } else if (aquifer_type=="unconfined") {
    dp <- -well$rate/(pi*Ksat)*log(r/well$roi)
  } else {
    stop("aquifer_type specified as:",aquifer_type,". It should be specified as \"confined\" or \"unconfined\".\n")
  }
  return(dp)
}

#' Get row of a data.frame as a vector
#'
#' return the row of a data.frame or tibble as a vector (used for extracting x,y coordinates)
get_row_as_vector <- function(df,row=1) {
  return(df %>% dplyr::slice(row) %>% unlist(.,use.names = FALSE))
}

#' Get cumulative effect of wells at location
#'
#' Get the cumulative effect of all wells at a singled location, output as head (confined aquifer) or discharge potential (unconfined aquifer).
#'
#' @param loc coordinates vector as c(x,y), with units of [m]
#' @inheritParams get_hydraulic_head
#' @return The output is the cumulative effect at \code{loc} of all \code{wells} on the hydraulic head [units=m] if
#'   \code{aquifer_type="confined"} or discharge potential [m^2] if
#'   \code{aquifer_type="unconfined"}.
#'
#'   Note: if the \code{loc} is contained within the diameter of any well, the distance between the location
#'   and that well is automatically adjusted to the edge of the well screen (i.e., well$diam/2).
#' @examples
#' wells <- data.frame(x0=c(0,0.5),y0=c(0,0.25),rate=c(1e-4,-2e-4),diam=c(0.75,0.8),roi=c(300,300))
#' get_potential(wells,loc=c(50,50),Ksat=0.00001,z0=10,aquifer_type="confined")
#' get_potential(wells,loc=c(50,50),Ksat=0.00001,aquifer_type="unconfined")
# get_potential <- function(loc,wells,Ksat,z0,aquifer_type) {
#   dp_vec <- sapply(split(wells,1:dim(wells)[1]),get_well_effect,loc=loc,Ksat=Ksat,z0=z0,aquifer_type=aquifer_type)
#   return(sum(dp_vec))
# }

#' Get cumulative effect of wells at location
#'
#' Get the hydraulic head at a location, accounting for the aquifer type
#' (confined or unconfined), resting head, and cumulative effect of all wells. This function
#' is a wrapper around \code{get_potential()} to get the actual hydraulic head instead of the differential potential.
#'
#' @param loc coordinates vector as c(x,y), with units of [m] or as data.frame with columns $x and $y
#' @param wells wells object with each row containing rate Q [m^3/s], diam [m],
#'   radius of influence roi [m], & coordinates x0 [m], y0 [m]
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
#'   \describe{ \item{aquifer_type="confined"}{\eqn{h=h_0+dP}}
#'   \item{aquifer_type="unconfined"}{\eqn{h=\sqrt{h_0^2+dP}}} }
#' @examples
#' wells <- data.frame(x0=c(0,0.5),y0=c(0,0.25),rate=c(1e-3,-2e-3),diam=c(0.75,0.8),roi=c(300,300))
#' get_hydraulic_head(wells,loc=c(5,5),h0=0,Ksat=0.00001,z0=30,aquifer_type="confined")
#' get_hydraulic_head(wells,loc=c(5,5),h0=30,Ksat=0.00001,aquifer_type="unconfined")
#'
#' grid_pts <- expand.grid(x=seq(0,10,by=5),y=seq(0,10,by=5))
#' get_hydraulic_head(wells,loc=grid_pts,h0=30,Ksat=0.00001,aquifer_type="unconfined")
get_hydraulic_head <- function(loc,wells,h0,Ksat,z0=NA,aquifer_type) {

  dP <- get_potential(loc,wells,Ksat=Ksat,z0=z0,aquifer_type=aquifer_type)
  #
  # loc_class <- class(loc)
  # # get change in potential due to wells
  # if (identical(loc_class,"numeric") | identical(loc_class,"integer")) { # loc is a vector as c(x, y)
  #   dP <- get_potential(loc,wells,Ksat=Ksat,z0=z0,aquifer_type=aquifer_type)
  # } else if (max(grepl("data.frame",class(loc)))) { # loc is a data.frame with $x and $y columns
  #   dP <- NULL
  #   loc_list <- lapply(split(loc %>% dplyr::select(x,y),1:dim(loc)[1]),get_row_as_vector)
  #   n <- length(loc_list)
  #
  #   if (n * dim(wells)[1] < 20) { # don't show progress bar for small number of points / wells
  #     for (i in 1:n) {
  #       dP[i] <- get_potential(loc_list[[i]],wells,Ksat=Ksat,z0=z0,aquifer_type=aquifer_type)
  #     }
  #   } else { # show progress bar for large number of points / wells
  #     start_time <- Sys.time()
  #     cat(paste0("Getting head at each point (",dim(loc)[1]," points, ",dim(wells)[1]," wells):\n"))
  #     pb <- txtProgressBar(min = 1, max = n, initial = 1, char = "=",width = NA, style = 3)
  #     for (i in 1:n) {
  #       dP[i] <- get_potential(loc_list[[i]],wells,Ksat=Ksat,z0=z0,aquifer_type=aquifer_type)
  #       setTxtProgressBar(pb, i)
  #     }
  #   }
  # }

  # calculate head using h0 and change in potential
  if (aquifer_type=="confined") {
    h <- h0 + dP
  } else if (aquifer_type=="unconfined") {
    h_squared <- pmax(h0^2 + dP,0)
    h <- sqrt(h_squared)
  } else {
    stop("aquifer_type specified as:",aquifer_type,". It should be specified as \"confined\" or \"unconfined\".\n")
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
#' @return Outputs the flow direction in the x and y directions. If the input \code{loc} is
#'   a numeric \code{c(x,y)}, then the output is in the same format. If the input is a data.frame,
#'   then the output is also a dataframe with columns \code{dx} and \code{dy}. The two values
#'   indicate the flow direction, and are equivalent to \eqn{-dh/dx}
#'   and \eqn{-dh/dy}.
#' @examples
#' wells <- data.frame(x0=c(0,0.5),y0=c(0,0.25),rate=c(1e-3,-2e-3),diam=c(0.75,0.8),roi=c(300,300))
#' get_flowdir(wells,loc=c(5,5),h0=0,Ksat=0.00001,z0=30,aquifer_type="confined")
#' get_flowdir(wells,loc=c(5,5),h0=30,Ksat=0.00001,aquifer_type="unconfined")
#'
#' grid_pts <- expand.grid(x=seq(0,10,by=5),y=seq(0,10,by=5))
#' get_flowdir(wells,loc=grid_pts,h0=30,Ksat=0.00001,aquifer_type="unconfined")
#'
#' # Injection and pumping well along diagonal line
#' wells2 <- data.frame(x0=c(-10,10),y0=c(-10,10),rate=c(1e-3,-1e-3),diam=c(0.1,0.1),roi=c(300,300))
#' grid_pts2 <- data.frame(x=c(-11,0,11),y=c(-11,0,11))
#' fd2 <- get_flowdir(wells2,loc=grid_pts2,h0=30,Ksat=0.00001,aquifer_type="unconfined")
#'
#' # Two pumping wells along diagonal line
#' wells3 <- data.frame(x0=c(-10,10),y0=c(-10,10),rate=c(-1e-3,-1e-3),diam=c(0.1,0.1),roi=c(300,300))
#' grid_pts3 <- data.frame(x=c(-3,-3,0,3,3),y=c(-3,3,0,-3,3))
#' fd3 <- get_flowdir(wells3,loc=grid_pts3,h0=30,Ksat=0.00001,aquifer_type="unconfined")
#'
#' ## plot the flow directions
#' # scale dx and dy for visualization
#' fd3_grid <- grid_pts3 %>% cbind(fd3) %>%
#'    dplyr::mutate(dx_norm=dx*50,dy_norm=dy*50,x2=x+dx_norm,y2=y+dy_norm) # normaliz
#' # alternatively, apply nonlinear scaling to dx and dy for visualization
#' fd3_grid <- grid_pts3 %>% cbind(fd3) %>%
#'   dplyr::mutate(angle=atan(dx/dy),mag=sqrt(dx^2+dy^2),mag_norm=mag^(1/2)*5,
#'                 dx_norm=mag_norm*cos(angle)*sign(dx),dy_norm=mag_norm*sin(angle)*sign(dx),
#'                 x2=x+dx_norm,y2=y+dy_norm)
#'
#' library(ggplot2)
#' ggplot(fd3_grid,aes(x,y)) + geom_point(size=2,shape=1) + geom_segment(aes(xend=x2,yend=y2),arrow=arrow(type="closed",length=unit(2,"mm"))) + coord_equal()
get_flowdir <- function(loc,wells,h0,Ksat,z0=NA,aquifer_type) {

  loc_class <- class(loc)
  # get change in potential due to wells
  if (identical(loc_class,"numeric") | identical(loc_class,"integer")) { # loc is a vector as c(x, y)
    fd <- -numDeriv::grad(get_hydraulic_head,loc,wells=wells,h0=h0,Ksat=Ksat,z0=z0,aquifer_type=aquifer_type)
  } else if (max(grepl("data.frame",class(loc)))) { # loc is a data.frame with $x and $y columns
    fd <- data.frame(dx=NULL,dy=NULL)
    loc_list <- lapply(split(loc %>% dplyr::select(x,y),1:dim(loc)[1]),get_row_as_vector)
    n <- length(loc_list)

    if (n * dim(wells)[1] < 20) { # no progress bar
      for (i in 1:n) {
        fd_i <- -numDeriv::grad(get_hydraulic_head,loc_list[[i]],wells=wells,h0=h0,Ksat=Ksat,z0=z0,aquifer_type=aquifer_type)
        fd_i_df <- data.frame(dx=fd_i[1],dy=fd_i[2])
        fd <- rbind(fd,fd_i_df)
      }
    } else { # with progress bar
      start_time <- Sys.time()
      cat(paste0("\nGetting flow direction at each point (",dim(loc)[1]," points, ",dim(wells)[1]," wells):\n"))
      pb <- txtProgressBar(min = 1, max = n, initial = 1, char = "=",width = NA, style = 3)
      for (i in 1:n) {
        fd_i <- -numDeriv::grad(get_hydraulic_head,loc_list[[i]],wells=wells,h0=h0,Ksat=Ksat,z0=z0,aquifer_type=aquifer_type)
        fd_i_df <- data.frame(dx=fd_i[1],dy=fd_i[2])
        fd <- rbind(fd,fd_i_df)
        setTxtProgressBar(pb, i)
      }
      cat("\n")
    }
  }

  return(fd)
}



##########################################################################################
############################################################################################################
############################################################################################################
# get potentials
# ##################
# a <- matrix(rnorm(1e4),nrow=100)
# b <- rowSums(matrix(rnorm(1e4),nrow=100))
# c_ <- a / b
# dim(c_)
#
# a <- matrix(rep(1:10,10),ncol=10)
# b <- matrix(1:10,nrow=10)
# # a %*% (1/b)
#
#
# nwells <- 10
# nobs <- 1000
#
# x_loc <- rnorm(nobs)
# y_obs <- rnorm(x_well)

# wells <- data.frame(x = rnorm(nwells), y = rnorm(nwells), roi = 1:nwells, diam = runif(nwells,0.3,2))

#


#' Get cumulative effect of wells at location
#'
#' Get the cumulative effect of all wells at a singled location, output as head (confined aquifer) or discharge potential (unconfined aquifer).
#'
#' @param loc coordinates \code{data.frame} with columns labeled 'x0' and 'y0', or as vector as c(x,y), with units of [m]
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
#' wells <- data.frame(x0=c(0,0.5),y0=c(0,0.25),rate=c(1e-4,-2e-4),diam=c(0.75,0.8),roi=c(300,300))
#' get_potential(wells,loc=c(50,50),Ksat=0.00001,z0=10,aquifer_type="confined")
#' get_potential(wells,loc=c(50,50),Ksat=0.00001,aquifer_type="unconfined")
#'
#' # Multiple test locations
#' wells <- data.frame(x0=c(-10,10),y0=c(-10,10),rate=c(1e-3,-1e-3),diam=c(0.1,0.1),roi=c(300,300))
#' grid_pts <- data.frame(x=c(-11,0,11),y=c(-11,0,11))
#' get_potentials(wells,grid_pts,Ksat=0.00001,aquifer_type="unconfined")
get_potential <- function(loc, wells, Ksat, z0, aquifer_type) {
  x_well <- wells$x
  y_well <- wells$y
  R <- wells$roi
  Q <- wells$rate
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
  if (aquifer_type=="confined") { # as a differential hydraulic head
    dP <- -rowSums(Qi/(2*pi*Ksat * z0)*log(rji/Ri))
  } else if (aquifer_type=="unconfined") { # as a differential discharge potential
    dP <- -rowSums(Qi/(pi*Ksat)*log(rji/Ri))
  }

  return(dP)
}