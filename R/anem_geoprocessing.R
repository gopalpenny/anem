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
#' @examples
#' boundaries <- data.frame(x1=c(0,10,13,1),y1=c(0,10,9,-1),x2=c(10,13,1,0),y2=c(10,9,-1,0))
#' ggplot(boundaries) + geom_segment(aes(x1,y1,xend=x2,yend=y2))
prep_bounds <- function(boundaries,get_rectangular=FALSE) {
  if (max(grepl("sf",class(boundaries)))) {
    boundaries <- prep_bounds_sf(boundaries)
  }
  if (!max(grepl("^bID$",names(boundaries)))) { # generate pID's if they are not present
    boundaries <- boundaries %>% dplyr::mutate(bID=dplyr::row_number())
  }
  if (get_rectangular) {
    bounds_w_slope <- get_rectangle(boundaries)
  } else{
    bounds_w_slope <- boundaries %>%
      dplyr::mutate(m=(y2-y1)/(x2-x1),
                    b=y1 - m*x1) %>% dplyr::select(-x1,-y1,-x2,-y2)
  }
  return(bounds_w_slope)
}


#' Convert quadrangle to rectangle
#'
#' Convert 4 lines to a rectangular box with only right angles
#' @param boundaries a \code{data.frame} containing 4 lines (rows) defined, respectively, by x1, y1, x2, y2, and an id (bID)
#' @importFrom magrittr %>%
#' @return Returns a \code{data.frame} containing slope and intercept for four edges of a rectangle. The
#' rectangle is determined by (1) identifying the quadrangle of the input \code{boundaries}, (2)
#' selecting the midpoints of each edge of the quadrangle, (3) determining the "long" axis of the
#' quadrangle, which becomes the long axis of the rectangle, (4) calculating the slope of the long
#' and short axes of the rectangle (at right angles), then generating lines with these slopes through
#' the midpoints.
#' @examples
#' boundaries <- data.frame(x1=c(0,10,13,1),y1=c(0,10,9,-1),x2=c(10,13,1,0),y2=c(10,9,-1,0)) %>%
#'   mutate(bID=row_number())
#' rect_boundaries <- get_rectangle(boundaries)
#' ggplot() + geom_segment(data=boundaries,aes(x1,y1,xend=x2,yend=y2)) +
#'   geom_abline(data=rect_boundaries,aes(slope=m,intercept=b,color=as.factor(bID)))
get_rectangle <- function(boundaries) {
  # get vertices
  quad_vertices_full <- get_quad_vertices(boundaries)

  # get midpoints of boundaries
  midpoints <- quad_vertices_full %>% dplyr::group_by(bID) %>%
    dplyr::summarize(x_mid=mean(x,na.rm=TRUE),
              y_mid=mean(y,na.rm=TRUE),
              opposite_bID=intersection_bID[is.na(x)])

  # get slope of of long and short axes
  slopes <- midpoints %>%
    dplyr::left_join(midpoints %>% dplyr::select(-bID) %>%
                       dplyr::rename(bID=opposite_bID,x_mid2=x_mid,y_mid2=y_mid)) %>%
    dplyr::mutate(dist=sqrt((x_mid2-x_mid)^2+(y_mid2-y_mid)^2))%>%
    dplyr::filter(dist==max(dist)) %>% dplyr::slice(1) %>%
    dplyr::mutate(m=(y_mid2-y_mid)/(x_mid2-x_mid),
                  m2=-1/m) %>%
    tidyr::gather(var,bID,dplyr::ends_with("bID")) %>%
    dplyr::select(bID,m,m2)

  # get boundaries as m, b
  bounds_rectangular <- midpoints %>%
    dplyr::left_join(slopes %>% dplyr::select(-m) %>% dplyr::rename(m=m2),by="bID") %>%
    dplyr::mutate(m=dplyr::if_else(!is.na(m),m,slopes$m[1])) %>%
    dplyr::mutate(b=y_mid - m*x_mid) %>% dplyr::select(m,b,bID)

  return(bounds_rectangular)
}

#' Get vertices of a quadrangle
#'
#' Get the 4 vertices correspondingt to quadrangle of 4 lines
#' @param boundaries a data.frame containing 4 lines (rows) defined, respectively, by x1, y1, x2, y2
#' @importFrom magrittr %>%
#' @return Returns a data.frame vertices for each of the 4 boundaries. The boundaries
#' are given an id, and there is one row in the output which identifies the opposide bound
#' (with which there is no intersection).
#' @examples
#' boundaries <- data.frame(x1=c(0,10,13,1),y1=c(0,10,9,-1),x2=c(10,13,1,0),y2=c(10,9,-1,0)) %>% mutate(bID=row_number())
#' quad_vertices <- get_quad_vertices(boundaries)
#' ggplot() + geom_segment(data=boundaries,aes(x1,y1,xend=x2,yend=y2)) +
#'   geom_point(data=quad_vertices,aes(x,y,shape=as.factor(bID),color=as.factor(bID)),size=4) +
#'   scale_shape_manual(values=1:4)
get_quad_vertices <- function(boundaries) {
  bounds_w_slope <- boundaries %>%
    dplyr::mutate(m=(y2-y1)/(x2-x1),
                  b=y1 - m*x1) %>% dplyr::select(-x1,-y1,-x2,-y2) %>%
    dplyr::arrange(desc(m))
  vertices <- bounds_w_slope %>%
    tidyr::crossing(bounds_w_slope %>% dplyr::rename(m2=m,b2=b,bID2=bID)) %>%
    dplyr::mutate(x=round(get_intersection(m,b,m2,b2)$x,14), # round to avoid math errors that make points seem different
           y=round(get_intersection(m,b,m2,b2)$y,14)) %>%
    dplyr::select(bID,bID2,x,y) %>% dplyr::filter(!is.nan(x))
  vertices_of_quad_midpoints <- vertices %>%
    dplyr::group_by(bID) %>%
    dplyr::summarize(x=get_point_on_quandrangle(x),
              y=get_point_on_quandrangle(y)) %>%
    dplyr::left_join(vertices,by=c("bID","x","y")) %>%
    tidyr::gather(orig,bID,dplyr::starts_with("bID"))
  segments_remaining <- vertices_of_quad_midpoints %>%
    dplyr::select(-orig) %>%
    dplyr::distinct() %>%
    dplyr::group_by(bID) %>%
    dplyr::summarize(n=dplyr::n()) %>%
    dplyr::filter(n==1)
  vertices_of_quad <- vertices %>%
    dplyr::filter(bID==segments_remaining$bID[1],bID2==segments_remaining$bID[2]) %>%
    tidyr::gather(orig,bID,dplyr::starts_with("bID")) %>%
    dplyr::bind_rows(vertices_of_quad_midpoints) %>%
    dplyr::select(-orig,-bID) %>% dplyr::distinct()# %>% mutate(keep=TRUE)
  quad_vertices_full <- vertices %>% dplyr::inner_join(vertices_of_quad,by=c("x","y")) %>%
    dplyr::rename(intersection_bID=bID2) %>%
    tidyr::complete(bID,intersection_bID) %>%
    dplyr::filter(bID!=intersection_bID)

  return(quad_vertices_full)
}


#' Get point on quadrangle
#'
#' The complete quadrangle is comprised of 6 vertices (intersection points)
#' of the edges of the quadrangle. But only 4 of these are the vertices of
#' the quadrangle. See http://mathworld.wolfram.com/CompleteQuadrilateral.html.
#' If a line has 3 intersection points, the middle one is definitely a vertex
#' of the quadrangle. If less than 3, then both are intersection points.
#' Choose the second one.
get_point_on_quandrangle <- function(vec) {
  if (length(vec)>1) {
    point <- sort(vec)[2]
  }
  return(point)
}

#' Get intersection of two lines specified by slope m and intercept, b
get_intersection <- function(m1,b1,m2,b2) {
  x <- (b2-b1)/(m1-m2)
  y <- m2*x + b2
  intersections <- data.frame(x=x,y=y)
  return(intersections)
}
