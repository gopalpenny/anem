# anem_preparation

#' Define wells
#'
#' Define wells in the aquifer with required parameters.
#'
#' @param wells_df A data.frame (or tibble, or sf with point geometry)
#'   containing properties of wells. If wells_df is an sf object, the
#'   coordinates for each well are extracted and the sf geometry is removed.
#' @param ... Named vector inputs to be added to the wells object. Names that
#'   replicate columns in \code{wells_df} supplant the columns in wells_df,
#'   except \code{wID} and \code{well_type} which are generated automatically.
#'   The length of inputs should generally be the same as the number of wells.
#'   However, if an input is repeated for all wells, it can be input with length 1.
#' @return This function returns a data.frame containing the columns:
#' \itemize{
#'  \item wID: Well ID
#'  \item Q: Pumping rate
#'  \item R: Radius of influence
#'  \item diam: Diameter of the well
#'  \item x, y: Cartesian coordinates
#'  \item well_type: Pumping (Q < 0), Injection (Q > 0), or Non-operational (Q = 0) well
#' }
#' These columns are followed by any other columns desired for the wells.
#' @importFrom magrittr %>%
#' @export
#' @examples
#' define_wells(Q=rep(0,3))
#'
#' wells_df <- data.frame(x=1:3,y=1:3,Q=11:13,diam=0.5,id=1001:1003)
#' wells <- define_wells(wells_df)
#' print(wells) # prints a tibble of the wells
#'
#' define_wells(wells_df,Note="note",Name=c("N1","N2","N3"))
#' define_wells(wells_df,R=1e4,Q=-21:-23) # include R and replace Q in wells_df
define_wells <- function(wells_df=NULL,...) {
  is_sf <- max(grepl("sf",class(wells_df)))
  params <- list(...)

  # Generate empty tibble with required column names
  columns <- c("wID", "Q", "R", "diam", "x", "y") %>%
    purrr::map_dfr( ~tibble::tibble(!!.x := numeric() ) )

  # Replicate inputs with length 1. Then check everything has the same length
  if (length(params) > 0) {
    num_wells <- c(wells_df=dim(wells_df)[1],sapply(params,length))
    replicate_wells <- function(val,max_wells) {
      if (length(val)==1) {
        return(rep(val,max_wells))
      } else {return(val)}
    }
    params <- lapply(params,replicate_wells,max_wells=max(num_wells))

    # Make sure that all inputs have 0, 1, or the same number of wells/elements
    if (!all(num_wells %in% c(0, 1, max(num_wells)))) {
      cat("The number of rows were specified for each input as:\n")
      print(num_wells)
      stop("All inputs should have the same number (or 0 or 1) of rows/elements.")
    }

    # generate wells_df if it is empty
    if (is.null(wells_df)) {
      wells_df <- columns %>% dplyr::bind_rows(data.frame(wID=rep(NA,max(num_wells))))
    }
  }

  # if wells_df is an sf object, extract x, y coordinates
  if (is_sf) {
    wells_crs <- sf::st_crs(wells_df)
    wells_df <- wells_df %>%
      dplyr::bind_cols(sf::st_coordinates(wells_df) %>% tibble::as_tibble()) %>%
      dplyr::rename(x=X,y=Y) %>% sf::st_set_geometry(NULL)
  }

  # Generate wells tibble
  wells <- dplyr::bind_rows(wells_df,columns)
  wells <- wells[,!(names(wells) %in% names(params))] %>%
    dplyr::bind_cols(params) %>%
    dplyr::mutate(wID=dplyr::row_number(),
                  well_type=factor(sign(Q),levels=-1:1,labels=c("Pumping","Non-operational","Injection")),
                  well_image="Actual") %>%
    dplyr::select(wID,Q,R,diam,x,y,well_type,well_image,dplyr::everything()) %>% tibble::as_tibble()

  if (is_sf) {
    wells <- wells %>% dplyr::mutate(X=x,Y=y) %>% sf::st_as_sf(coords=c("X","Y"),crs=wells_crs)
  }

  return(wells)
}


#' Define boundaries with slope and intercept
#'
#' Prepare boundaries with slope (m) and intercept (b) of all line objects, and add boundary ID (bID)
#'
#' @param bounds An sf object of straight lines (single segments), a
#'   data.frame with column names x1, y1, x2, y2, representing the endpoints,
#'   or a data.frame with columns m (slope) and b (intercept)
#' @return A data.frame containing slope (m) and intercept (b) for each line,
#'   along with original columns. If \code{bounds} is an sf object, the columns
#'   x1, y1, x2, y2 are automatically extracted using \code{prep_wells_sf} and
#'   added to the sf object before getting the slope and intercept. The function requires
#'   that the wells are labeled with a column of identifiers, pID. If it is not
#'   present, the function generates them.
#' @importFrom magrittr %>%
#' @export
#' @examples
#' bounds_df <- data.frame(bound_type=c("CH","NF","NF","NF"),x1=c(0,10,13,1),y1=c(0,10,9,-1),x2=c(10,13,1,0),y2=c(10,9,-1,0))
#' bounds <- define_bounds(bounds_df)
#' ggplot(bounds) + geom_segment(aes(x1,y1,xend=x2,yend=y2)) + coord_equal()
#'
#' bounds_df <- data.frame(bound_type=c("CH","CH","NF","NF"),x1=c(0,0,10,10),y1=c(0,10,10,0),x2=c(0,10,10,0),y2=c(10,10,0,0))
#' bounds <- define_bounds(bounds_df)
#' ggplot(bounds) + geom_segment(aes(x1,y1,xend=x2,yend=y2)) + coord_equal()
define_bounds <- function(bounds_df,get_rectangular=TRUE) {
  is_sf <- max(grepl("sf",class(bounds_df)))
  bounds_prep <- bounds_df
  if (!max(grepl("^bID$",names(bounds_df)))) { # generate bID's if they are not present
    bounds_prep <- bounds_prep %>% dplyr::mutate(bID=dplyr::row_number())
  }

  if (is.factor(bounds_prep$bound_type)) {
    bounds_prep$bound_type <- as.character(bounds_prep$bound_type)
  }

  # Generate empty tibble with required column names
  columns <- tibble::tibble(bID=as.integer(),bound_type=as.character()) %>% cbind(
    c("x1", "y1", "x2", "y2") %>% purrr::map_dfr( ~tibble::tibble(!!.x := numeric() ) ) )

  # Get x, y coordinates and strip sf
  if (is_sf) {
    bounds_crs <- sf::st_crs(bounds_prep)
    bounds_prep <- bounds_prep %>% dplyr::select(-dplyr::matches("^x1$"),-dplyr::matches("^x2$"),-dplyr::matches("^y1$"),-dplyr::matches("^y2$"))
    bounds_prep <- prep_bounds_sf(bounds_prep) %>% sf::st_set_geometry(NULL)
    # boundaries_no_geometry <- bounds_prep %>% sf::st_set_geometry(NULL) %>%
    #   dplyr::select(-x1,-x2,-y1,-y2)
  }

  # get rectangular bounds
  if (get_rectangular) {
    bounds <- bounds_prep %>%
      dplyr::select(-dplyr::matches("^x1$"),-dplyr::matches("^x2$"),-dplyr::matches("^y1$"),-dplyr::matches("^y2$"),-dplyr::matches("^m$"),-dplyr::matches("^b$")) %>%
      dplyr::left_join(get_rectangle(bounds_prep),by="bID")
  } else{
    bounds <- bounds_prep %>%
      dplyr::mutate(m=(y2-y1)/(x2-x1),
                    b=y1 - m*x1)
  }

  bounds <- columns %>% dplyr::bind_rows(bounds) %>%
    dplyr::select(bID,bound_type,m,b,x1,y1,x2,y2,dplyr::everything()) %>%
    tibble::as_tibble()

  # add back sf properties
  if (is_sf) {
    bounds <- bounds_to_sf(bounds,bounds_crs)
  }

  bcheck <- check_bounds(bounds)
  return(bounds)
}


#' Define aquifer recharge
#'
#' Define recharge as undisturbed water table gradients
#' @param type Type of recharge gradient. One of "F" (uniform flow),
#' H (head boundaries), or D (recharge divide)
#' @param recharge_vector A numeric vector containing \code{c(x1, y1, x2, y2)}
#' @param aquifer Aquifer containing \code{aquifer_type}, \code{Ksat}, and if necessary \code{h0} and \code{z0}
#' @param ... See details for required and optional parameters
#' @details
#' This function defines recharge to the aquifer by defining the undisturbed water
#' table profile. Each of the following describes the required parameters for \code{recharge_type}
#' within the \code{...} input:
#' \describe{
#' \item{Constant flow, "F"}{This allows a uniform constant flow in the direction of the
#' \code{recharge_vector}. Flow is specified as cumec/m, where the length dimension in the
#' denominator is perpendicular to the recharge vector. This \code{recharge_type} requires parameters:
#' \itemize{
#' \item \code{x0, y0}: Coordinate locations where undisturbed head is equal to \code{aquifer$h0}
#' \item \code{flow}: Flow in cumec
#' }
#' }
#' \item{Head boundaries, "H"}{NOT YET IMPLEMENTED. This allows the head profile to be specified by 2 or 3 points,
#' where the result is steady uniform flow determined by the hydraulic gradient between head at the \code{recharge_vector}
#' origin and head at the other 1 (or 2) point(s). Note that \code{aquifer$h0} is ignored with this option.
#' This \code{recharge_type} requires parameters:
#' \itemize{
#' \item \code{h1}: Hydraulic head at \code{x1, y1}
#' \item \code{h2}: Hydraulic head at \code{x2, y2}
#' \item \code{x3, y3, h3}: Optional coordinate location \code{x3, y3} and hydraulic head \code{h3} at a third point.
#' If this is specified, there will be a watershed divide
#' }
#' }
#' \item{Recharge divide, "D"}{NOT YET IMPLEMENTED. This allows a uniform constant flow in opposite directions on
#' both sides of a watershed divide. The divide goes through the \code{recharge_vector} origin \code{x1, y1}
#' and is perpendicular to the \code{recharge_vector}. Flow on both sides specified as cumec/m, where the length dimension in the
#' denominator is parallel to the to the recharge divide This \code{recharge_type} requires parameters:
#' \itemize{
#' \item \code{x0, y0}: Coordinate locations where undisturbed head is equal to \code{aquifer$h0}
#' \item \code{flow}: Flow in the direction of the \code{recharge_vector}
#' \item \code{flow2}: Flow in the opposite direction of the \code{recharge_vector} (other side of the divide).
#' }
#' }
#' }
#' @return
#' The function returns a list containing two elements:
#' \describe{
#' \item{recharge_params}{A list of all required parameters for recharge_func, as described above in the "Details" section.}
#' \item{recharge_func}{A function that takes \code{recharge_params} and {aquifer} as inputs}
#' }
#' @examples
#' aquifer <- define_aquifer("confined",Ksat=0.001,z0=10,h0=100)
#' input_params <- list(flow=1,x0=3,y0=3)
#' define_recharge(recharge_type="F",recharge_vector=c(0,0,3,3),aquifer=aquifer,flow=1,x0=3,y0=3)
define_recharge <- function(recharge_type, recharge_vector, aquifer, ...) {
  if (length(recharge_vector) != 4 | class(recharge_vector)!="numeric") {
    stop("recharge_vector must be numeric vector of length 4.")
  }
  input_params <- list(...)

  print(names(input_params))
  # recharge_type <- input_params$recharge_type
  x1 <- recharge_vector[1]
  y1 <- recharge_vector[2]
  x2 <- recharge_vector[3]
  y2 <- recharge_vector[4]

  if (recharge_type == "F" & aquifer$aquifer_type=="confined") {
    if (!all(c("flow","x0","y0") %in% names(input_params))) {
      stop("For recharge type == \"F\", argument ... must contain flow, x0, y0")
    }
    dx <- x2 - x1
    dy <- y2 - y1
    theta <- atan(abs(dy/dx))
    x_term <- - input_params$flow * cos(theta) * sign(dx) / (aquifer$Ksat * aquifer$z0)
    y_term <- - input_params$flow * sign(theta) * sign(dy) / (aquifer$Ksat * aquifer$z0)
    output_params <- c(list(recharge_type=recharge_type,x1=x1,y1=y1,x2=x2,y2=y2),
                       input_params,
                       list(scenario="cF",x_term=x_term, y_term=y_term))
    # recharge_func <- get_recharge_potential_confined_flow
  } else {
    stop(paste0("This combination of aquifer_type (",aquifer_type,") and recharge type (",recharge_type,")",
                "is currently not supported."))
  }

  return(as.data.frame(output_params))
}



#' Define aquifer
#'
#' Define an aquifer as confined or unconfined and with saturated hydraulic conductivity.
#' Other optional parameters can be defined including boundaries and recharge.
#'
#' @param aquifer_type "confined" or "unconfined"
#' @param Ksat Saturated hydraulic conductivity
#' @param ... Optional parameters including boundaries, recharge zone, and aquifer thickness
#' @return This function returns an S3 "aquifer" object that behaves as a list and contains
#'  the named items:
#' \itemize{
#'  \item aquifer_type: "confined" or "unconfined"
#'  \item Ksat: Saturated hydraulic conductivity
#'  \item h0: Hydraulic head of the undisturbed aquifer (i.e., no pumping)
#'  \item z0: Thickness of the aquifer (applies only to confined aquifers)
#'  \item bounds: Boundaries of the aquifer, if any (generated using \code{define_boundaries()})
#' }
#' @importFrom magrittr %>%
#' @export
#' @examples
#' (aquifer <- define_aquifer("confined",1e-4))
#'
#' bounds_df1 <- data.frame(bound_type=c("CH","NF","NF","NF"),x1=c(0,10,13,1),y1=c(0,10,9,-1),x2=c(10,13,1,0),y2=c(10,9,-1,0))
#' aquifer_confined <- define_aquifer("confined",1e-3,bounds=bounds_df1,h0=100,z0=10)
#' print(aquifer_confined)
#'
#' bounds_df2 <- data.frame(bound_type=c("CH","CH","NF","NF"),x1=c(0,0,10,10),y1=c(0,10,10,0),x2=c(0,10,10,0),y2=c(10,10,0,0))
#' aquifer_confined <- define_aquifer("unconfined",1e-3,bounds=bounds_df2,h0=100)
#' aquifer_confined
define_aquifer <- function(aquifer_type,Ksat,...) {
  params <- list(...)
  aquifer_params <- params

  if (!is.null(params$bounds)) {
    aquifer_params$bounds <- define_bounds(params$bounds)
  }

  aquifer <- c(list(aquifer_type=aquifer_type,Ksat=Ksat),aquifer_params)

  class(aquifer) <- "aquifer"
  return(aquifer)
}
