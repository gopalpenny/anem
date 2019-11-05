# aem_geoprocessing.R
# This file contains code for prepping and geoprocessing spatial features for the anelem package.


#' Get UTM zone from coordinates
#'
#' @param lon longitude in [-180,180]
#' @return The number of the UTM zone containing the longitude
#' @examples
#' longitude_to_utm_zone(-45)
longitude_to_utm_zone <- function(lon) {
  utm_zone <- (base::floor((lon + 180)/6) %% 60) + 1
  return(utm_zone)
}

#' Convert utm zone to proj4string
#'
#' @param utm_zone UTM zone as numeric integer, or string
#' @return The proj4string of the UTM Zone
#' @examples
#' utm_zone_to_proj4(32)
utm_zone_to_proj4 <- function(utm_zone) {
  proj4_base <- "+proj=utm +zone=UTM_ZONE +datum=WGS84 +units=m +no_defs"
  return(gsub("UTM_ZONE",as.character(utm_zone),proj4_base))
}

#' Convert wells sf object to dataframe
#'
#' Convert wells sf object to dataframe, with appropriate column coordinates
#'
#' @param wells wells sf object, with point features
#' @param results outputs a data.frame of wells with all of the attributes and coordinate as x0, y0
#' @importFrom magrittr %>%
prep_wells_sf <- function(wells_sf) {
  wells_df <- wells_sf %>%
    dplyr::bind_cols(sf::st_coordinates(wells_sf) %>% tibble::as_tibble()) %>%
    dplyr::rename(x0=X,y0=Y)
  return(wells_df)
}

#' Convert boundaries sf object to dataframe, with appropriate column coordinates
#'
#' @param bounds_sf boundaries sf object, with line features (2 end points)
#' @return outputs a data.frame with all of the attributes and coordinate endpoints as x1, y1, x2, y2.
#' @importFrom magrittr %>%
prep_bounds_sf <- function(bounds_sf) {
  bounds_sf <- bounds_sf %>% dplyr::mutate(L1=dplyr::row_number())
  bounds_prep <- bounds_sf %>%
    sf::st_coordinates() %>% tibble::as_tibble() %>% dplyr::group_by(L1) %>%
    dplyr::mutate(col_x=factor(dplyr::row_number(),levels=1:2,labels=c("x1","x2")),
                  col_y=gsub("x","y",col_x)) #%>%
  bounds_prep2 <- bounds_prep %>% dplyr::select(X,L1,col_x) %>% tidyr::spread(col_x,X) %>%
    dplyr::full_join(bounds_prep %>% dplyr::select(Y,L1,col_y) %>% tidyr::spread(col_y,Y),by="L1")
  bounds_df <- bounds_sf %>% dplyr::left_join(bounds_prep2,by="L1") %>%
    dplyr::select(-L1) #%>% sf::st_set_geometry(NULL)
  return(bounds_df)
}

#' Prepare boundaries with slope and intercept
#'
#' Get slope (m) and intercept (b) of all line objects, and add boundary ID (bID)
#'
#' @param boundaries An sf object of straight lines (single segments), or a
#'   data.frame with column names x1, y1, x2, y2, representing the endpoints
#' @return A data.frame containing slope (m) and intercept (b) for each line,
#'   along with original columns. If \code{boundaries} is a data.frame, the columns
#'   x1, y1, x2, y2 are automatically calculated using \code{prep_wells_sf} and
#'   added to the sf object before getting the slope and intercept. The function requires
#'   that the wells are labeled with a column of identifiers, pID. If it is not
#'   present, the function generates them.
#' @importFrom magrittr %>%
prep_bounds <- function(boundaries) {
  if (max(grepl("sf",class(boundaries)))) {
    boundaries <- prep_bounds_sf(boundaries)
  }
  if (!max(grepl("^bID$",names(boundaries)))) { # generate pID's if they are not present
    boundaries <- boundaries %>% dplyr::mutate(bID=dplyr::row_number())
  }
  bounds_w_slope <- boundaries %>%
    dplyr::mutate(m=(y2-y1)/(x2-x1),
           b=y1 - m*x1) %>% dplyr::select(-x1,-y1,-x2,-y2)
  return(bounds_w_slope)
}
