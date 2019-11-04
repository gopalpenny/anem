# anema_potentials.R

# grid_bounds <- bounds %>% sf::st_coordinates() %>% tibble::as_tibble() %>%
#   dplyr::summarize(xmin=min(X),xmax=max(X),ymin=min(Y),ymax=max(Y))
# grid_pts <- with(grid_bounds,dplyr::crossing(x=seq(xmin,xmax,length.out=10),y=seq(ymin,ymax,length.out=10)))


get_well_effect <- function(well,point,Ksat,roi,z0=NULL,aquifer_type) {
  # cat("Make sure units are correct, including coordinates. Suggest m, m^2, m^3, s.\n")
  # well: well sf object with each feature containing rate Q [L^3/T], radius of influence R [L], & coordinates x0, y0
  # point: c(x,y) coordinates vector -- MUST BE IN SAME LENGTH UNITS AS T, Q
  # Ksat: saturated hydraulic conductivity, needed in all cases
  # z0: needed only in the confined case
  # output: do is the change in potential in same units of length as everything else (L^2 for unconfined aquifer)
  x1 <- point[1]
  y1 <- point[2]
  x0 <- well$x0
  y0 <- well$y0
  r <- sqrt((x1-x0)^2+(y1-y0)^2)

  if (r > roi) {
    dp <- 0
  } else if (aquifer_type=="confined") {
    if(is.null(z0)) {
      stop("For confined aquifer, must specify z0")
    }
    # Transmissivity: as Ksat * z0, the thickness of the aquifer [L^2/T]. Must have same length and time units as Q
    Transmissivity <- Ksat * z0
    dp <- -well$rate/(2*pi*Transmissivity)*log(r/roi)
  } else if (aquifer_type=="unconfined") {
    dp <- -well$rate/(pi*Ksat)*log(r/roi)
  } else {
    stop("aquifer_type specified as:",aquifer_type,". It should be specified as \"confined\" or \"unconfined\".\n")
  }
  return(dp)
}

get_row_as_vector <- function(tibble,row) {
  return(tibble %>% slice(row) %>% unlist(.,use.names = FALSE))
}

# get_well_effect(wells[1,],get_row_as_vector(grid_pts,1),2,300000,aquifer_type="unconfined")

get_potential <- function(point,wells,aquifer_type) {
  # point: c(x,y) coordinates vector
  # wells: wells sf object with each feature containing rate & coordinates x0, y0
  # aquifer_type: "confined" or "unconfined"
  dp_list <- sapply(split(pts,1:dim(pts)[1]),get_well_effect,point=get_row_as_vector(grid_pts,1),Ksat=2,aquifer_type=aquifer_type)
}
