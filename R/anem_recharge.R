# anem_recharge.R

#' Get hydraulic potential due to recharge
#'
#' Get hydraulic potential due to recharge
#' @param loc Location for evaluating Vector location as c(x,y) or data.frame containing x, y columns
#' @param Aquifer Aquifer containing containing \code{h0} and \code{recharge$params} (see Details)
#' @details
#' The aquifer and aquifer$recharge must be correctly specified using \code{define_aquifer} and/or \code{define_recharge}.
#' @export
#' @return
#' Returns hydraulic potential at the location(s) using helper functions for each recharge_type / aquifer_type:
#' \itemize{
#' \item 1a. Flow, confined aquifer: \code{get_recharge_flow_potential()}
#' }
#' @examples
#' ## Flow - confined aquifer
#' recharge_params <- list(recharge_type="F",recharge_vector=c(0,0,1,1),flow=1,x0=0,y0=0)
#' aquifer <- define_aquifer("confined",1,h0=0,z0=1,recharge=recharge_params)
#' get_recharge_hydraulic_potential(c(1/sqrt(2),1/sqrt(2)), aquifer)
#'
#' loc <- expand.grid(x=-2:2,y=-2:2)
#' loc$h <- get_recharge_hydraulic_potential(loc, aquifer)
#' library(ggplot2)
#' ggplot(loc) + geom_raster(aes(x,y,fill=h)) + scale_fill_gradient2()
#'
#' recharge_params <- list(recharge_type="F",recharge_vector=c(-1,-5,0,0),flow=1,x0=0,y0=0)
#' aquifer <- define_aquifer("confined",1,h0=10,z0=1,recharge=recharge_params)
#' loc <- expand.grid(x=-100:100,y=-100:100)
#' loc$h <- get_recharge_hydraulic_potential(loc, aquifer)
#' ggplot(loc) + geom_raster(aes(x,y,fill=h)) + scale_fill_gradient2()
get_recharge_hydraulic_potential <- function(loc, aquifer) {

  # 1. Recharge type: Flow
  if (aquifer$recharge$recharge_type == "F") {
    # 1a. Confined, 1b. Unconfined aquifers
    pot <- get_recharge_flow_potential(loc, aquifer)
  }

  if (is.null(pot)) {
    stop("error processing potential")
  }
  return(pot)
}


#' Recharge flow potential function
#'
#' Recharge flow potential function
#' @param loc Location for evaluating Vector location as c(x,y) or data.frame containing x, y columns
#' @param Aquifer Aquifer containing containing \code{aquifer_type},\code{h0} and \code{recharge} (see Details)
#' @details
#' Recharge parameters must contain \code{x0}, \code{y0}, \code{x_term}, and \code{y_term}. For confined aquifers,
#' the last two are:
#' \itemize{
#' \item \code{x_term}: -flow * cos(theta) * sign(dx) / (Ksat * z0)
#' \item \code{y_term}: -flow * sin(theta) * sign(dy) / (Ksat * z0)
#' }
#' For unconfined aquifers,
#' these parameters are:
#' \itemize{
#' \item \code{x_term}: -2 * flow * cos(theta) * sign(dx) / Ksat
#' \item \code{y_term}: -2 * flow * sin(theta) * sign(dy) / Ksat
#' }
#' @return
#' Returns hydraulic head (confined aquifers) or discharge potential (unconfined aquifers) at the location(s).
#' @examples
#' ## Flow - confined aquifer
#' recharge_params <- list(recharge_type="F",recharge_vector=c(0,0,1,2),flow=1,x0=0,y0=0)
#' aquifer <- define_aquifer("confined",1,h0=0,z0=1,recharge=recharge_params)
#' get_recharge_flow_potential(c(1/sqrt(2),1/sqrt(2)), aquifer)
#'
#' loc <- expand.grid(x=-2:2,y=-2:2)
#' loc$h <- get_recharge_flow_potential(loc, aquifer)
#' library(ggplot2)
#' ggplot(loc) + geom_raster(aes(x,y,fill=h)) + scale_fill_gradient2()
#'
#' recharge_params <- list(recharge_type="F",recharge_vector=c(-1,-5,0,0),flow=1,x0=0,y0=0)
#' aquifer <- define_aquifer("confined",1,h0=10,z0=1,recharge=recharge_params)
#' loc <- expand.grid(x=-100:100,y=-100:100)
#' loc$h <- get_recharge_flow_potential(loc, aquifer)
#' ggplot(loc) + geom_raster(aes(x,y,fill=h)) + scale_fill_gradient2()
#'
#' recharge_params <- list(recharge_type="F",recharge_vector=c(-1,-5,0,0),flow=1,x0=0,y0=0)
#' aquifer <- define_aquifer("unconfined",1e-1,h0=50,recharge=recharge_params)
#' loc <- expand.grid(x=-100:100,y=-100:100)
#' loc$h2 <- get_recharge_flow_potential(loc, aquifer)
#' loc <- loc %>% dplyr::mutate(h=sqrt(h2))
#' ggplot(loc) + geom_raster(aes(x,y,fill=h)) + scale_fill_gradient2(midpoint=50)
get_recharge_flow_potential <- function(loc, aquifer) {
  params <- aquifer$recharge

  # 1b Confined flow
  if (aquifer$aquifer_type=="confined") {
    if (class(loc) == "numeric") {
      pot <- aquifer$h0 + params$x_term * (loc[1] - params$x0) + params$y_term * (loc[2] - params$y0)
    } else if (any(grepl("data.frame",class(loc)))) {
      pot <- aquifer$h0 + params$x_term * (loc$x - params$x0) + params$y_term * (loc$y - params$y0)
    } else {
      stop("loc must be numeric vector of length 2 or data.frame containin x, y columns.")
    }

  # 1b Unconfined flow
  } else if (aquifer$aquifer_type=="unconfined") {
    if (class(loc) == "numeric") {
      pot <- aquifer$h0^2 + params$x_term * (loc[1] - params$x0) + params$y_term * (loc[2] - params$y0)
    } else if (any(grepl("data.frame",class(loc)))) {
      pot <- aquifer$h0^2 + params$x_term * (loc$x - params$x0) + params$y_term * (loc$y - params$y0)
    } else {
      stop("loc must be numeric vector of length 2 or data.frame containin x, y columns.")
    }
  }

  return(pot)
}


#' Recharge divide potential function
#'
#' Recharge divide potential function
#' @param loc Location for evaluating Vector location as c(x,y) or data.frame containing x, y columns
#' @param Aquifer Aquifer containing containing \code{aquifer_type},\code{h0} and \code{recharge} (see Details)
#' @details
#' Recharge parameters must contain \code{x0}, \code{y0}, \code{x_term}, and \code{y_term}. For confined aquifers,
#' the last two are:
#' \itemize{
#' \item \code{x_term}: -flow * cos(theta) * sign(dx) / (Ksat * z0)
#' \item \code{y_term}: -flow * sin(theta) * sign(dy) / (Ksat * z0)
#' }
#' For unconfined aquifers,
#' these parameters are:
#' \itemize{
#' \item \code{x_term}: -2 * flow * cos(theta) * sign(dx) / Ksat
#' \item \code{y_term}: -2 * flow * sin(theta) * sign(dy) / Ksat
#' }
#' @return
#' Returns hydraulic head (confined aquifers) or discharge potential (unconfined aquifers) at the location(s).
#' @examples
#' ## Flow - confined aquifer
#' recharge_params <- list(recharge_type="D",recharge_vector=c(0,0,1,2),flow=1,x0=0,y0=0)
#' aquifer <- define_aquifer("confined",1,h0=0,z0=1,recharge=recharge_params)
#' get_recharge_divide_potential(c(1/sqrt(2),1/sqrt(2)), aquifer)
#'
#' loc <- expand.grid(x=-2:2,y=-2:2)
#' loc$h <- get_recharge_divide_potential(loc, aquifer)
#' library(ggplot2)
#' ggplot(loc) + geom_raster(aes(x,y,fill=h)) + scale_fill_gradient2()
#'
#' recharge_params <- list(recharge_type="D",recharge_vector=c(-1,-5,0,0),flow=1,x0=0,y0=0)
#' aquifer <- define_aquifer("confined",1,h0=10,z0=1,recharge=recharge_params)
#' loc <- expand.grid(x=-100:100,y=-100:100)
#' loc$h <- get_recharge_divide_potential(loc, aquifer)
#' ggplot(loc) + geom_raster(aes(x,y,fill=h)) + scale_fill_gradient2()
#'
#' recharge_params <- list(recharge_type="D",recharge_vector=c(0,0,1,1),flow_main=1,flow_opp=1,x0=0,y0=0)
#' aquifer <- define_aquifer("confined",1e-1,h0=0,z0=10,recharge=recharge_params)
#' loc <- c(10,10)
#' get_recharge_divide_potential(c(5,5), aquifer)
#' loc <- expand.grid(x=-100:100,y=-100:100)
#' loc$h2 <- get_recharge_divide_potential(loc, aquifer)
#' loc <- loc %>% dplyr::mutate(h=sqrt(h2))
#' ggplot(loc) + geom_raster(aes(x,y,fill=h)) + scale_fill_gradient2(midpoint=50)
get_recharge_divide_potential <- function(loc, aquifer) {
  params <- aquifer$recharge

  # 1b Confined flow
  if (aquifer$aquifer_type=="confined") {
    if (class(loc) == "numeric") {
      # Check which side of the divide the point is on -- main or opposite
      # Point is on main side (direction of recharge_vector) IF:
      # (a) y_side == main_side_y, OR (b) abs(m) == Inf & sign(x - b) ==
      # Otherwise, the point is on the divide or on the opposite side.
      y_side <- sign(loc[2] - params$divide_m * loc[1] + params$divide_b)
      x_side_Inf <- sign(loc[1] - params$divide_b)
      if (y_side == params$main_side_y | (x_side_Inf == params$main_side_x & abs(params$divide_m) == Inf)) {
        pot <- params$h0_divide + params$x_term_main * (loc[1] - params$x1) + params$y_term_main * (loc[2] - params$y1)
      } else {
        pot <- params$h0_divide + params$x_term_opp * (loc[1] - params$x1) + params$y_term_opp * (loc[2] - params$y1)
      }
    } else if (any(grepl("data.frame",class(loc)))) {
      df <- loc %>% dplyr::mutate(y_side = sign(y - params$divide_m * x + params$divide_b),
                                  x_side = sign(x - params$divide_b)) %>%
        dplyr::mutate(main=dplyr::case_when(
        y_side == params$main_side_y~TRUE,
        x_side_Inf == params$main_side_x & abs(params$divide_m) == Inf~TRUE,
        TRUE~FALSE
      )) %>% dplyr::mutate(x_term = ifelse(main_side,x_term_main,x_term_opp),
                           y_term = ifelse(main_side,y_term_main,y_term_opp),
                           pot = params$h0_divide + x_term * (y - params$x1) + y_term * (x - params$y1))
    } else {
      stop("loc must be numeric vector of length 2 or data.frame containin x, y columns.")
    }

    # 1b Unconfined flow
  } else if (aquifer$aquifer_type=="unconfined") {
    if (class(loc) == "numeric") {
      pot <- aquifer$h0^2 + params$x_term * (loc[1] - params$x0) + params$y_term * (loc[2] - params$y0)
    } else if (any(grepl("data.frame",class(loc)))) {
      pot <- aquifer$h0^2 + params$x_term * (loc$x - params$x0) + params$y_term * (loc$y - params$y0)
    } else {
      stop("loc must be numeric vector of length 2 or data.frame containin x, y columns.")
    }
  }

  return(pot)
}


