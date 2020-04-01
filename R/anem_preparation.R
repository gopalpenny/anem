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
#' define_wells(data.frame(wID=c(3,NA,9)),Q=rep(0,3))
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
  wells_prep <- dplyr::bind_rows(wells_df,columns)
  wells_prep2 <- wells_prep[,!(names(wells_prep) %in% names(params))] %>%
    dplyr::bind_cols(params) %>%
    dplyr::mutate(wID=as.integer(wID)) # ensure wID is an integer before checking for duplicates
  # Set wID
  if (any(duplicated(wells_prep2$wID[!is.na(wells_prep2$wID)]))) {
    warning("Some wIDs are duplicates. Replacing all wIDs with row_number")
    wells_prep2$wID <- 1:nrow(wells_prep2)
  } else if (any(is.na(wells_prep2$wID))) {
    for (i in 1:nrow(wells_prep2)) {
      if (is.na(wells_prep2$wID[i])) {
        wells_prep2$wID[i] <- max(c(0,wells_prep2$wID),na.rm=TRUE) + 1
      }
    }
  }
  wells <- wells_prep2 %>%
    dplyr::mutate(wID=as.integer(wID),
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
#' @param bounds_df An sf object of straight lines (single segments), a
#'   data.frame with column names x1, y1, x2, y2, representing the endpoints,
#'   or a data.frame with columns m (slope) and b (intercept)
#' @param get_rectangular logical that, if \code{TRUE}, forces bounds to be rectangular
#' @return A data.frame containing slope (m) and intercept (b) for each line,
#'   along with original columns. If \code{bounds} is an sf object, the columns
#'   x1, y1, x2, y2 are automatically extracted using \code{prep_wells_sf} and
#'   added to the sf object before getting the slope and intercept. The function requires
#'   that the wells are labeled with a column of identifiers, pID. If it is not
#'   present, the function generates them.
#' @importFrom magrittr %>%
#' @export
#' @examples
#' bounds_df <- data.frame(bound_type=c("CH","NF","NF","NF"),
#'   x1=c(0,10,13,1),y1=c(0,10,9,-1),
#'   x2=c(10,13,1,0),y2=c(10,9,-1,0))
#' bounds <- define_bounds(bounds_df)
#'
#'
#' library(ggplot2)
#' ggplot(bounds) + geom_segment(aes(x1,y1,xend=x2,yend=y2)) + coord_equal()
#'
#' bounds_df <- data.frame(bound_type=c("CH","CH","NF","NF"),
#'   x1=c(0,0,10,10),y1=c(0,10,10,0),
#'   x2=c(0,10,10,0),y2=c(10,10,0,0))
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
#' Define recharge as undisturbed water table gradients. Note that recharge acts as if the
#' aquifer boundaries did not exist. Therefore, when parameterizing recharge, care must be taken
#' to ensure only plausible scenarios.
#' @param recharge_params A list containing any of the named or unnamed (i.e, in \code{...}) input parameters.
#' If any named arguments are missing, they will be replaced by items in this list.
#' @param recharge_type Type of recharge gradient. One of "F" (uniform flow),
#' "H" (head boundaries), or "D" (recharge divide)
#' @param recharge_vector A numeric vector containing \code{c(x1, y1, x2, y2)}
#' @param aquifer Aquifer containing \code{aquifer_type}, \code{Ksat}, and if necessary \code{h0} and \code{z0}
#' @param ... See details for required and optional parameters
#' @details
#' This function defines recharge to the aquifer by defining the undisturbed water
#' table profile. In addition to the named parameters, each \code{recharge_type} has additional
#' parameters that must be specified as named arguments in \code{recharge_params} or \code{...}:
#' \describe{
#' \item{Constant flow, "F"}{This allows a uniform constant flow in the direction of the
#' \code{recharge_vector}. Flow is specified as cumec/m, where the length dimension in the
#' denominator is perpendicular to the recharge vector. This \code{recharge_type} requires parameters:
#' \itemize{
#' \item \code{x0, y0}: Coordinate locations where undisturbed head is equal to \code{aquifer$h0}
#' \item \code{flow}: Flow in cumec, per m (perpendicular to the flow vector)
#' }
#' The function returns a list of named parameters including:
#' \itemize{
#' \item All of the input parameters
#' \item \code{scenario}: A string combining "c" or "u" for aquifer type, and the recharge type
#' \item \code{x_term}, \code{y_term}: flow direction. See Value for details.
#' }
#' }
#' \item{Recharge divide, "D"}{This allows a uniform constant flow in opposite directions on
#' both sides of a divide. The divide goes through the \code{recharge_vector} origin \code{x1, y1}
#' and is perpendicular to the \code{recharge_vector}. Flow on either side is specified as cumec/m, where the length dimension in the
#' denominator is parallel to the to the recharge divide. This \code{recharge_type} requires parameters:
#' \itemize{
#' \item \code{x0, y0}: Coordinate locations where undisturbed head is equal to \code{aquifer$h0}
#' \item \code{flow_main}: Flow in cumec in the direction of the \code{recharge_vector}, per m.
#' \item \code{flow_opp}: Flow in cumec the opposite direction of the \code{recharge_vector}, per m.
#' Positive value means flow away from divide.
#' }
#' The function returns a list of named parameters including:
#' \itemize{
#' \item All of the input parameters
#' \item \code{scenario}: A string combining "c" or "u" for aquifer type, and the recharge type
#' \item \code{h0_divide}: Hydraulic head at the divide
#' \item \code{divide_m}, \code{divide_b}: Slope and aspect of dividing line
#' \item \code{main_side_x}, \code{main_side_y}: Direction (+1 or -1) of flow_main relative to divide
#' \item \code{x_term_main}, \code{y_term_main}: \code{x_term} and \code{y_term} for main direction of flow. See Value for details.
#' \item \code{x_term_opp}, \code{y_term_opp}: \code{x_term} and \code{y_term} for opposite direction of flow. See Value for details.
#' }
#' }
#' }
#' @section Potential future implementation:
#' \describe{
#' \item{Head boundaries, "H"}{This is not yet implemented. It would allow the head profile to be specified by 2 or 3 points,
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
#' }
#' @export
#' @return
#' The function returns a list containing the elements described in Details.
#' The parameters \code{x_term} and \code{y_term} specify the flow direction, and their
#' calculation depends on \code{aquifer_type} as follows:
#' \describe{
#' \item{"confined"}{\eqn{x_term, y_term ~ - Q / (Ksat z0)}. They are equivalent to dh/dx and dh/dy.
#' Head differential in the x-direction with respect to some x = x0, h = h0 is therefore
#' \eqn{h - h0 = (x-x0) x_term}.
#' Flow per unit length in the x-direction is \eqn{Qx/L = -x_term Ksat z0}. They are defined as:
#' \itemize{
#' \item \code{x_term}: -flow * cos(theta) * sign(dx) / (Ksat * z0)
#' \item \code{y_term}: -flow * sin(theta) * sign(dy) / (Ksat * z0)
#' }
#' where sin(theta) * sign(dx) is the component of \code{recharge_vector} in the x direction.
#' }
#' \item{"unconfined"}{\eqn{x_term, y_term ~ - 2 Q / Ksat}. They are equivalent to dh^2/dx and dh^2/dy.
#' Change in discharge potential in the x-direction with respect to some x = x0, h = h0 is therefore
#' \eqn{h^2- h0^2 = (x-x0) x_term}. They are defined as:
#' \itemize{
#' \item \code{x_term}: -2 * flow * cos(theta) * sign(dx) / Ksat
#' \item \code{y_term}: -2 * flow * sin(theta) * sign(dy) / Ksat
#' }
#' where sin(theta) * sign(dx) is the component of \code{recharge_vector} in the x direction.
#' }
#' }
#' @examples
#' aquifer <- define_aquifer("confined",Ksat=0.001,z0=10,h0=100)
#' recharge_params <- list(recharge_type="F",recharge_vector=c(0,0,3,3),flow=1,x0=3,y0=3)
#' define_recharge(recharge_params,aquifer=aquifer)
#'
#' recharge_params <- list(recharge_type="F",recharge_vector=c(0,0,3,3),aquifer=aquifer,flow=1,x0=3,y0=3)
#' define_recharge(recharge_params)
#'
#' aquifer <- define_aquifer("unconfined",Ksat=0.001,h0=100)
#' recharge_params <- list(recharge_type="F",recharge_vector=c(0,0,3,3),flow=1,x0=3,y0=3)
#' define_recharge(recharge_params, aquifer=aquifer)
#'
#' aquifer <- define_aquifer("confined",Ksat=1,z0=10,h0=0)
#' recharge_params <- list(recharge_type="D",recharge_vector=c(0,0,1,1),aquifer=aquifer,flow_main=1,flow_opp=1,x0=1,y0=1)
#' define_recharge(recharge_params)
define_recharge <- function(recharge_params, recharge_type, recharge_vector, aquifer, ...) {
  # If any inputs are missing, replace values with those from recharge_params
  if (missing(recharge_type)) {
    recharge_type <- recharge_params$recharge_type
  }
  if (missing(recharge_vector)) {
    recharge_vector <- recharge_params$recharge_vector
  }
  if (missing(aquifer)) {
    aquifer <- recharge_params$aquifer
  }

  if (length(recharge_vector) != 4 | class(recharge_vector)!="numeric") {
    stop(paste0("recharge_vector must be numeric vector of length 4. Recharge vector is: ",cat(recharge_vector)))
  }
  # for debugging only:
  if (interactive()) { # add unnamed recharge_params to input_params
    input_params <- recharge_params[-which(names(recharge_params) %in% c("recharge_type","recharge_vector","aquifer"))]
  }
  if (missing(recharge_params)) {
    input_params <- list(...)
  } else {  # add unnamed recharge_params to input_params
    input_params <- c(list(...),recharge_params[-which(names(recharge_params) %in% c("recharge_type","recharge_vector","aquifer"))])
  }

  x1 <- recharge_vector[1]
  y1 <- recharge_vector[2]
  x2 <- recharge_vector[3]
  y2 <- recharge_vector[4]

  # 1. Recharge type: Flow
  if (recharge_type == "F") {
    if (!all(c("flow","x0","y0") %in% names(input_params))) {
      stop("For recharge type == \"F\", argument ... must contain flow, x0, y0")
    }
    dx <- x2 - x1
    dy <- y2 - y1
    theta <- atan(abs(dy/dx))

    if (aquifer$aquifer_type=="confined") {
      if (!all(c("Ksat","z0") %in% names(aquifer))) {
        stop("For recharge type == \"F\", confined aquifer must contain Ksat, z0")
      }
      x_term <- - input_params$flow * cos(theta) * sign(dx) / (aquifer$Ksat * aquifer$z0)
      y_term <- - input_params$flow * sin(theta) * sign(dy) / (aquifer$Ksat * aquifer$z0)
      scenario <- "cF"
    } else if (aquifer$aquifer_type=="unconfined") {
      if (!all(c("Ksat") %in% names(aquifer))) {
        stop("For recharge type == \"F\", unconfined aquifer must contain Ksat")
      }
      x_term <- - 2 * input_params$flow * cos(theta) * sign(dx) / aquifer$Ksat
      y_term <- - 2 * input_params$flow * sin(theta) * sign(dy) / aquifer$Ksat
      scenario <- "uF"
    }
    # Flow output recharge_params
    output_params <- c(list(recharge_type=recharge_type,x1=x1,y1=y1,x2=x2,y2=y2),
                       input_params,
                       list(scenario="cF",x_term=x_term, y_term=y_term))

  # 2. Recharge type: Divide
  } else if (recharge_type == "D") {
    if (!all(c("flow_main","flow_opp","x0","y0") %in% names(input_params))) {
      stop("For recharge type == \"D\", argument ... must contain flow_main, flow_opp, x0, y0")
    }

    recharge_line <- get_slope_intercept(x1,y1,x2,y2)
    recharge_divide <- get_perpendicular_line(recharge_line$m,x1,y1)
    main_side <- c(sign(x2-x1),sign(y2-y1)) # side of divide where recharge_vector is defined

    dx <- x2 - x1
    dy <- y2 - y1
    theta <- atan(abs(dy/dx))

    if (aquifer$aquifer_type=="confined") {
      if (!all(c("Ksat","z0") %in% names(aquifer))) {
        stop("For recharge type == \"D\", confined aquifer must contain Ksat, z0")
      }
      x_term_main <- - input_params$flow_main * cos(theta) * sign(dx) / (aquifer$Ksat * aquifer$z0)
      y_term_main <- - input_params$flow_main * sin(theta) * sign(dy) / (aquifer$Ksat * aquifer$z0)
      x_term_opp <- input_params$flow_opp * cos(theta) * sign(dx) / (aquifer$Ksat * aquifer$z0)
      y_term_opp <- input_params$flow_opp * sin(theta) * sign(dy) / (aquifer$Ksat * aquifer$z0)
      h0_divide <- aquifer$h0 + x_term_main * (x1-input_params$x0) + y_term_main * (y1-input_params$y0)
      scenario <- "cD"
    } else if (aquifer$aquifer_type=="unconfined") {
      if (!all(c("Ksat") %in% names(aquifer))) {
        stop("For recharge type == \"D\", unconfined aquifer must contain Ksat")
      }
      x_term_main <- - 2 * input_params$flow_main * cos(theta) * sign(dx) / aquifer$Ksat
      y_term_main <- - 2 * input_params$flow_main * sin(theta) * sign(dy) / aquifer$Ksat
      x_term_opp <- 2 * input_params$flow_opp * cos(theta) * sign(dx) / aquifer$Ksat
      y_term_opp <- 2 * input_params$flow_opp * sin(theta) * sign(dy) / aquifer$Ksat
      h0_divide <- sqrt(aquifer$h0^2 + x_term_main * (x1-input_params$x0) + y_term_main * (y1-input_params$y0))
      scenario <- "uD"
    }
    # Divide output recharge_params
    output_params <- c(list(recharge_type=recharge_type,x1=x1,y1=y1,x2=x2,y2=y2),
                       input_params,
                       list(scenario="cF", h0_divide=h0_divide, divide_m = recharge_divide$m, divide_b=recharge_divide$b,
                            main_side_x = main_side[1], main_side_y = main_side[2],
                            x_term_main=x_term_main, y_term_main=y_term_main,x_term_opp=x_term_opp, y_term_opp=y_term_opp))
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
#' @param ... Optional parameters including boundaries, recharge_params, and aquifer thickness
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
#' (aquifer <- define_aquifer("confined",1e-4,h0=100,z0=10))
#'
#' bounds_df1 <- data.frame(bound_type=c("CH","NF","NF","NF"),x1=c(0,10,13,1),y1=c(0,10,9,-1),x2=c(10,13,1,0),y2=c(10,9,-1,0))
#' aquifer_confined <- define_aquifer("confined",1e-3,bounds=bounds_df1,h0=100,z0=10)
#' print(aquifer_confined)
#'
#' bounds_df2 <- data.frame(bound_type=c("CH","CH","NF","NF"),x1=c(0,0,10,10),y1=c(0,10,10,0),x2=c(0,10,10,0),y2=c(10,10,0,0))
#' aquifer_confined <- define_aquifer("unconfined",1e-3,bounds=bounds_df2,h0=100)
#' aquifer_confined
#'
#' recharge_params <- list(recharge_type="F",recharge_vector=c(0,0,3,3),flow=1,x0=3,y0=3)
#' aquifer <- define_aquifer("confined",1e-3,h0=50,z0=10,recharge=recharge_params)
define_aquifer <- function(aquifer_type,Ksat,...) {
  params <- list(...)
  aquifer_params <- params[!(names(params) %in% c("bounds","recharge"))]

  aquifer_prep <- c(list(aquifer_type=aquifer_type,Ksat=Ksat),aquifer_params)

  # define boundaries
  if (!is.null(params$bounds)) {
    aquifer_prep$bounds <- define_bounds(params$bounds)
  }

  # define recharge
  if (!is.null(params$recharge)) {
    aquifer_prep$recharge <- define_recharge(params$recharge,aquifer = aquifer_prep)
  }

  aquifer <- aquifer_prep
  class(aquifer) <- "aquifer"
  return(aquifer)
}
