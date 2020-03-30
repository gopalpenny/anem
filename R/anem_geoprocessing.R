# anem_geoprocessing.R
# This file contains code for prepping and geoprocessing spatial features for the anelem package.

#' Get UTM zone from coordinates
#'
#' @param lon longitude in [-180,180]
#' @return The number of the UTM zone containing the longitude
#' @export
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
#' @export
#' @examples
#' utm_zone_to_proj4(32)
utm_zone_to_proj4 <- function(utm_zone) {
  proj4_base <- "+proj=utm +zone=UTM_ZONE +datum=WGS84 +units=m +no_defs"
  return(gsub("UTM_ZONE",as.character(utm_zone),proj4_base))
}

#' Convert boundaries sf object to dataframe, with appropriate column coordinates
#'
#' @param bounds_sf boundaries sf object, with line features (2 end points)
#' @keywords internal
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

#' Convert quadrangle to rectangle
#'
#' Convert 4 lines to a rectangular box with only right angles
#' @param bounds a \code{data.frame} containing 4 lines (rows) defined by columns bID and, either x1, y1, x2, and y2, or m and b
#' @importFrom magrittr %>%
#' @return Returns a \code{data.frame} containing slope and intercept for four edges of a rectangle. The
#' rectangle is determined by (1) identifying the quadrangle of the input \code{bounds}, (2)
#' selecting the midpoints of each edge of the quadrangle, (3) determining the "long" axis of the
#' quadrangle, which becomes the long axis of the rectangle, (4) calculating the slope of the long
#' and short axes of the rectangle (at right angles), then generating lines with these slopes through
#' the midpoints.
#' @export
#' @keywords internal
#' @examples
#' \dontrun{
#' library(tidyverse)
#' bounds <- data.frame(x1=c(0,10,13,1),y1=c(0,10,9,-1),x2=c(10,13,1,0),y2=c(10,9,-1,0))
#' rect_boundaries <- get_rectangle(bounds)
#' ggplot() + geom_segment(data=bounds,aes(x1,y1,xend=x2,yend=y2)) +
#'   geom_segment(data=rect_boundaries,aes(x1,y1,xend=x2,yend=y2,color=as.factor(bID))) + coord_equal()
#'
#' bounds2 <- data.frame(m=c(2,4,-1,-1),b=c(0,4,3,20))
#' rect_boundaries2 <- get_rectangle(bounds2)
#' ggplot() + geom_abline(data=bounds2,aes(slope=m,intercept=b)) +
#'   geom_segment(data=rect_boundaries2,aes(x1,y1,xend=x2,yend=y2,color=as.factor(bID))) + coord_equal()
#'
#'
#' bounds3 <- data.frame(m=c(Inf,0,Inf,0),b=c(0,4,4,0))
#' rect_boundaries3 <- get_rectangle(bounds3)
#' ggplot() + geom_abline(data=bounds3 %>% filter(m!=Inf),aes(slope=m,intercept=b),linetype="dashed") +
#'   geom_vline(data=bounds3 %>% filter(m==Inf),aes(xintercept=b),linetype="dashed") +
#'   geom_segment(data=rect_boundaries3,aes(x1,y1,xend=x2,yend=y2,color=as.factor(bID))) + coord_equal()
#'
#' bounds4 <- data.frame(bID=1:4,x1=c(0,0,10,10),y1=c(0,10,10,0),x2=c(0,10,10,0),y2=c(10,10,0,0))
#' rect_boundaries4 <- get_rectangle(bounds4)
#' ggplot() + geom_segment(data=bounds4,aes(x1,y1,xend=x2,yend=y2)) +
#'   geom_segment(data=rect_boundaries4,aes(x1,y1,xend=x2,yend=y2,color=as.factor(bID))) + coord_equal()
#' }
get_rectangle <- function(bounds) {
  is_sf <- max(grepl("sf",class(bounds)))

  ## GENERATE BOUNDS OF SF
  if (is_sf & !any(all(c("m","b") %in% names(bounds)),
           all(c("x1","x2","y1","y2") %in% names(bounds)))) {
    # NOT YET WORKING
    bounds <- prep_bounds_sf(bounds)
  }

  if (is.null(bounds$bID)) {
    bounds <- bounds %>%
      dplyr::mutate(bID=dplyr::row_number())
  }

  # get vertices (and remove extraneous properties, including sf)
  quad_vertices_full <- get_quad_vertices(bounds)

  # get midpoints of bounds
  midpoints <- quad_vertices_full %>% #dplyr::select(-intersection_bID) %>%
    dplyr::distinct() %>% dplyr::group_by(bID) %>%
    dplyr::summarize(x_mid=mean(x,na.rm=TRUE),
              y_mid=mean(y,na.rm=TRUE),
              opposite_bID=intersection_bID[is.na(x)])

  # get slope of of long and short axes
  slopes <- midpoints %>%
    dplyr::left_join(midpoints %>% dplyr::select(-bID) %>%
                       dplyr::rename(bID=opposite_bID,x_mid2=x_mid,y_mid2=y_mid),by="bID") %>%
    dplyr::mutate(dist=sqrt((x_mid2-x_mid)^2+(y_mid2-y_mid)^2)) %>%
    dplyr::filter(dist==max(dist)) %>% dplyr::slice(1) %>%
    dplyr::mutate(m=get_slope_intercept(x_mid,y_mid,x_mid2,y_mid2)$m,
                  m2=get_slope_intercept(y_mid,-x_mid,y_mid2,-x_mid2)$m) %>%
    tidyr::gather(var,bID,dplyr::ends_with("bID")) %>%
    dplyr::select(bID,m,m2)

  # get bounds as m, b
  bounds_rectangular <- midpoints %>%
    dplyr::left_join(slopes %>% dplyr::select(-m) %>% dplyr::rename(m=m2) %>% dplyr::mutate(bGroup=1),by="bID") %>%
    dplyr::mutate(m=dplyr::if_else(!is.na(m),m,slopes$m[1]),
                  bGroup=dplyr::if_else(!is.na(bGroup),bGroup,2)) %>%
    dplyr::mutate(b=get_slope_intercept(x_mid,y_mid,m=m)$b) %>% dplyr::select(m,b,bID)


  quad_vertices_final <- get_quad_vertices(bounds_rectangular) %>%
    dplyr::filter(!is.na(x)) %>% dplyr::select(x,y,bID) %>%
    dplyr::distinct() %>%
    dplyr::group_by(bID)
  quad_vertices_final_wide <- quad_vertices_final %>%
    dplyr::mutate(pt_num=dplyr::row_number()) %>% dplyr::group_by() %>%
    tidyr::gather(var,val,x,y) %>%
    tidyr::unite("pt",var,pt_num,sep = "") %>%
    tidyr::spread(pt,val) %>% dplyr::select(bID,x1,y1,x2,y2)

  bounds_rectangular <- bounds_rectangular %>% dplyr::left_join(quad_vertices_final_wide,by="bID")

  if(is_sf) {
    quad_vertices_final_sf <- quad_vertices_final %>%
      sf::st_as_sf(coords=c("x","y")) %>%
      sf::st_set_crs(sf::st_crs(bounds))
    quad_vertices_final_linestring <- quad_vertices_final_sf %>% dplyr::group_by(bID) %>%
      dplyr::summarize(geometry=sf::st_union(geometry)) %>%
      sf::st_cast("LINESTRING",union=TRUE) %>%
      sf::st_set_crs(sf::st_crs(bounds))
    bounds_rectangular <- quad_vertices_final_linestring %>%
      dplyr::left_join(bounds_rectangular,by="bID")
  }

  return(bounds_rectangular)
}

#' Get bounds as sf object
#'
#' Convert bounds object with bID, x1, y1, x2, y2 to sf object
#' @keywords internal
#' @examples
#' \dontrun{
#' bounds <- data.frame(bID=1,x1=1,y1=1,x2=3,y2=3)
#' bounds_to_sf(bounds,crs=4326)
#' }
bounds_to_sf <- function(bounds,crs) {
  bounds_geometry <- bounds %>% tidyr::gather(coord,val,x1,y1,x2,y2) %>%
    dplyr::mutate(axis=substr(coord,1,1),
                  pt_num=substr(coord,2,2)) %>%
    dplyr::select(-coord) %>% tidyr::spread(axis,val) %>%
    sf::st_as_sf(coords=c("x","y")) %>% sf::st_set_crs(crs) %>%
    dplyr::group_by(bID) %>%
    dplyr::summarize() %>% sf::st_cast("LINESTRING")
  bounds_with_sf <- bounds_geometry %>% dplyr::left_join(bounds,by="bID") %>%
    dplyr::select(!! names(bounds),dplyr::everything())
  return(bounds_with_sf)
}

#' Get vertices of a quadrangle
#'
#' Get the 4 vertices corresponding to quadrangle of 4 lines
#' @param bounds a data.frame containing 4 lines (rows) defined by bID and x1, y1, x2, y2, or m and b
#' @importFrom magrittr %>%
#' @keywords internal
#' @return Returns a data.frame vertices for each of the 4 bounds. The bounds
#' are given an id, and there is one row in the output which identifies the opposide bound
#' (with which there is no intersection).
#' @examples
#' \dontrun{
#' library(tidyverse)
#' bounds <- data.frame(x1=c(0,10,13,1),y1=c(0,10,9,-1),x2=c(10,13,1,0),y2=c(10,9,-1,0)) %>% mutate(bID=row_number())
#' quad_vertices <- get_quad_vertices(bounds)
#' ggplot() + geom_segment(data=bounds,aes(x1,y1,xend=x2,yend=y2)) +
#'   geom_point(data=quad_vertices,aes(x,y,shape=as.factor(bID),color=as.factor(bID)),size=4) +
#'   scale_shape_manual(values=1:4)
#'
#' bounds <- data.frame(x1=c(0,1,2,1),y1=c(1,2,1,0),x2=c(1,2,1,0),y2=c(2,1,0,1)) %>% mutate(bID=row_number())
#' quad_vertices <- get_quad_vertices(bounds)
#' ggplot() + geom_segment(data=bounds,aes(x1,y1,xend=x2,yend=y2)) + coord_equal()
#'   geom_point(data=quad_vertices,aes(x,y,shape=as.factor(bID),color=as.factor(bID)),size=4) +
#'   scale_shape_manual(values=1:4)
#'
#' bounds <- data.frame(bID=1:4,x1=c(0,0,10,10),y1=c(0,10,10,0),x2=c(0,10,10,0),y2=c(10,10,0,0))
#' quad_vertices <- get_quad_vertices(bounds)
#' ggplot() + geom_segment(data=bounds,aes(x1,y1,xend=x2,yend=y2)) + coord_equal() +
#'   geom_point(data=quad_vertices,aes(x,y,shape=as.factor(bID),color=as.factor(bID)),size=4) +
#'   scale_shape_manual(values=1:4)
#'
#' bounds <-data.frame(bID=c(5, 6, 7, 8),
#'                     x1=c(468888, 572670, 483978, 468888),
#'                     x2=c(572670, 588746, 588746, 483978),
#'                     y1=c(4592114, 4630407, 4518117, 4592114),
#'                     y2=c(4630407, 4556624, 4556624, 4518117))
#' vertices <- get_quad_vertices(bounds)
#' }
get_quad_vertices <- function(bounds) {
  if (max(grepl("sf",class(bounds)))) {
    bounds <- bounds %>% sf::st_set_geometry(NULL)
  }
  if (min(c('m','b') %in% names(bounds))) {
    bounds_w_slope <- bounds %>% dplyr::select(m,b,bID)
  } else if (min(c('x1','x2','y1','y2') %in% names(bounds))) {
    bounds_w_slope <- bounds %>%
      dplyr::mutate(m=get_slope_intercept(x1,y1,x2,y2)$m,
                    b=get_slope_intercept(x1,y1,x2,y2)$b) %>% dplyr::select(m,b,bID)
  }
  vertices <- bounds_w_slope %>%
    tidyr::crossing(bounds_w_slope %>% dplyr::rename(m2=m,b2=b,bID2=bID)) %>%
    dplyr::mutate(x=round(get_intersection(m,b,m2,b2)$x,10), # round to avoid math errors that make points seem different
           y=round(get_intersection(m,b,m2,b2)$y,10)) %>%
    dplyr::select(bID,bID2,x,y) %>% dplyr::filter(!is.nan(x),abs(x)!=Inf)
  vertices_of_quad_midpoints <- vertices %>%
    dplyr::group_by(bID) %>%
    dplyr::summarize(X=get_point_on_quandrangle(x,y,"x"),
                     Y=get_point_on_quandrangle(x,y,"y")) %>%
    dplyr::rename(x=X,y=Y) %>%
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
  quad_vertices_full <- vertices %>% dplyr::mutate(x=signif(x,10),y=signif(y,10)) %>%
    dplyr::inner_join(signif(vertices_of_quad,10),by=c("x","y")) %>% # select vertices in the quadrangle
    dplyr::select(-x,-y) %>%
    dplyr::left_join(vertices,by=c("bID","bID2")) %>%
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
#' @keywords internal
get_point_on_quandrangle <- function(x,y,axis) {
  # if (length(x)>1) {
    vec_order <- order(x)
    x <- x[vec_order]
    y <- y[vec_order]
  # }
  if (axis=="x") {
    point <- x[2]
  } else if (axis=="y") {
    point <- y[2]
  }
  return(point)
}

#' Get intersection of two lines
#'
#' Get intersection of two lines specified by slope m and intercept, b
#' @examples
#' get_intersection(1,0,-1,2)
#' get_intersection(Inf,1,0,2)
#' get_intersection(Inf,1,Inf,2)
get_intersection <- function(m1,b1,m2,b2) {
  x <- ifelse(m1==m2,NaN,
              ifelse(m1==Inf, b1,
                     ifelse(m2==Inf, b2,
                            (b2-b1)/(m1-m2))))
  y <- ifelse(m1==m2,NaN,
              ifelse(m1==Inf, m2*x + b2,
                     ifelse(m2==Inf, m1*x + b1,
                            m2*x + b2)))
  intersections <- data.frame(x=x,y=y)
  return(intersections)
}

#' Get nearest point on line
#'
#' Get nearest point on
#' @param loc Points as \code{c(x,y)} or a \code{data.frame} with columns \code{x} and \code{y}
#' @param m Slope of the line
#' @param b Intercept of the line (if \code{m} is \code{Inf}, \code{b} should be the x-intercept)
#' @keywords internal
#' @examples
#'
#' \dontrun{
#' get_nearest_point_on_boundary(c(1,1),)
#' bounds <- data.frame(bound_type=c("CH","NF","NF","NF"),m=c(Inf,0,Inf,0),b=c(0,0,100,100)) %>% define_bounds()
#' loc <- c(150,150)
#' get_nearest_point_on_line(loc,bounds[1,]$m,bounds[1,]$b)
#'
#' bounds <- data.frame(bound_type=c("CH","NF","NF","NF"),m=c(-2,0.5,-2,0.5),b=c(0,0,100,20)) %>% define_bounds()
#' loc <- data.frame(x=c(-100,0,100),y=c(-150,0,150))
#' nearest <- get_nearest_point_on_line(loc,bounds[1,]$m,bounds[1,]$b)
#' ggplot() +
#'    geom_segment(data=bounds,aes(x1,y1,xend=x2,yend=y2))  +
#'    geom_abline(data=bounds[1,],aes(slope=m,intercept=b),linetype="dashed")  +
#'    geom_point(data=loc,aes(x,y),color="black")  +
#'    geom_point(data=nearest,aes(x,y),color="red",alpha=0.5) +
#'    coord_equal()
#'
#' loc_rep <- data.frame(x=rep(0,4),y=rep(75,4))
#' nearest <- get_nearest_point_on_line(loc_rep,m=bounds$m,b=bounds$b)
#' ggplot() +
#'    geom_segment(data=bounds,aes(x1,y1,xend=x2,yend=y2))  +
#'    geom_abline(data=bounds,aes(slope=m,intercept=b),linetype="dashed")  +
#'    geom_point(data=loc_rep,aes(x,y),color="black")  +
#'    geom_point(data=nearest,aes(x,y),color="red",alpha=0.5) +
#'    coord_equal()
#' }
get_nearest_point_on_line <- function(loc,m,b) {
  if (max(grepl("data.frame",class(loc)))) {
    x_loc <- loc$x
    y_loc <- loc$y
  } else {
    x_loc <- loc[1]
    y_loc <- loc[2]
  }
  if (!(length(m) %in% c(1,length(x_loc))) | !(length(b) %in%  c(1,length(x_loc)))) {
    stop("get_nearest_point_on_line: m has length ",length(m),
         ", b has length",length(b),". The length of each should be 1 or the number of points.")
  }
  if (length(m) == 1) {
    m <- rep(m,length(x_loc))
  } else if (length(m)!=length(x_loc)) {
    stop("get_nearest_point_on_line: m has length ",length(m),
         ", b has length",length(b),". The length of each should be 1 or the number of points.")
  }
  if (length(b) == 1) {
    b <- rep(b,length(x_loc))
  } else if (length(b)!=length(x_loc)) {
    stop("get_nearest_point_on_line: m has length ",length(m),
         ", b has length",length(b),". The length of each should be 1 or the number of points.")
  }

  x <- ifelse(m==Inf,b,
              ifelse(m==0, x_loc,
                     get_intersection(m,b,-1/m, y_loc  + x_loc/m)$x))
  y <- ifelse(m==Inf,y_loc,
              ifelse(m==0, b,
                     get_intersection(m,b,-1/m, y_loc + x_loc/m)$y))
  nearest_point <- data.frame(x=x,y=y)
  return(nearest_point)
}

#' Generate outline of circle
#'
#' @param circle A \code{data.frame} with $x, $y, and $r (or $R) columns. $id optional
#' @keywords internal
#' @importFrom magrittr %>%
gen_circleFun <- function(circle, npoints = 100){
  center <- c(circle$x,circle$y)
  if("r" %in% names(circle)) {
    r <- circle$r
  } else if("R" %in% names(circle)) {
    r <- circle$R
  } else {
    stop("Need to properly define the circle radius as $r or $R.")
  }
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  df <- data.frame(x = xx, y = yy)
  if (!is.null(circle$id)) {
    df <- df %>% dplyr::mutate(id=circle$id)
  }
  return(df)
}

#' Generate an object that acts like a circle
#' @keywords internal
gen_circ <- function(x,y,r) {
  return(list(x=x,y=y,r=r))
}

#' Generate an object that acts like a circle
#'
#' @param df data.frame with columns x, y, r
#' @export
#' @examples
#' library(tidyverse)
#' df <- data.frame(x=1:3,y=1:3,roi=c(0.5,1,1.5))
#' circles <- gen_circles(df %>% rename(r=roi))
#' ggplot(circles) + geom_polygon(aes(x,y,group=id),color="black",alpha=0.5) + coord_equal()
gen_circles <- function(df) {
  df$id <- 1:dim(df)[1]
  circles <- do.call(rbind,lapply(split(df,df$id),gen_circleFun))
  return(circles)
}

#' Get quadrangle vertices
#'
#' Get quadrangle vertices in wide format
#' @keywords internal
get_quad_vertices_wide <- function(bounds) {
  bounds <- get_quad_vertices(bounds) %>% dplyr::filter(!is.na(x)) %>% dplyr::group_by(bID) %>%
    dplyr::mutate(num=rank(intersection_bID)) %>% dplyr::select(-intersection_bID) %>%
    tidyr::gather(axis,val,x,y) %>% tidyr::unite(pt,axis,num) %>% tidyr::spread(pt,val)
  return(bounds)
}

#' Get slope and intercept
#'
#' Get slope and intercept of a line defined by x1, y1, x2, y2
#' @keywords internal
#' @examples
#' \dontrun{
#' get_slope_intercept(0,0,1,2)
#' get_slope_intercept(0,-0,2,-1)
#' get_slope_intercept(0,0,0,3)
#' get_slope_intercept(0,2,3,2)
#' get_slope_intercept(1,1,m=-1)
#' get_slope_intercept(1,1,m=Inf)
#' }
get_slope_intercept <- function(x1,y1,x2=NULL,y2=NULL,m=NULL) {
  if (is.null(m)) {
    m <- dplyr::if_else(x2==x1, Inf, (y2-y1)/(x2-x1))
    b <- dplyr::if_else(x2==x1, x1, y1 - m*x1)
  } else {
    b <- dplyr::if_else(abs(m)==Inf, x1, y1 - m*x1)
  }
  return(list(m=m,b=b))
}




#' Get distance from point to bounds
#'
#' Get distance from point to bounds. Location on the boundaries
#' has to fall within the segment x1, y1, x2, y2
#' @param loc Location given as c(x,y) or as data.frame and $x, $y
#' @param bounds Boundary object with m and b or x1, y1, x2, y2
#' @importFrom magrittr %>%
#' @keywords internal
#' @examples
#' \dontrun{
#' library(tidyverse)
#' bounds <- data.frame(bound_type=c("CH","NF","NF","NF"),m=c(Inf,0,Inf,0),b=c(0,0,100,100)) %>% define_bounds()
#' loc <- c(150,150)
#' get_distance_to_bounds(loc,bounds)
#'
#' bounds <- data.frame(bound_type=c("CH","NF","NF","NF"),m=c(-2,0.5,-2,0.5),b=c(0,0,100,20)) %>% define_bounds()
#' loc <- data.frame(x=c(-200,0,200,50),y=c(-200,0,200,-50))
#' get_distance_to_bounds(loc,bounds)
#' ggplot(bounds) +
#'   geom_segment(data=bounds,aes(x1,y1,xend=x2,yend=y2)) +
#'   geom_point(data=loc,aes(x,y)) +
#'   coord_equal()
#' }
get_distance_to_bounds <- function(loc,bounds,return_locations=FALSE) {
  if (any(grepl("data.frame",class(loc)))) {
    x_loc <- loc$x
    y_loc <- loc$y
  } else {
    x_loc <- loc[1]
    y_loc <- loc[2]
  }
  dist <- rep(NA,length(x_loc))

  bound_vertices <- get_quad_vertices(bounds) %>%
    dplyr::filter(!is.na(x)) %>% dplyr::select(bound_x=x,bound_y=y) %>% dplyr::distinct()

  # for each point
  for (i in 1:length(x_loc)) {
    nearest_on_line <- get_nearest_point_on_line(data.frame(x=rep(x_loc[i],length(bounds$m)),y=y_loc[i]),
                                                 bounds$m,bounds$b) %>% dplyr::rename(xL=x,yL=y)
    nearest_on_bounds <- nearest_on_line %>%
      dplyr::bind_cols(bounds) %>% dplyr::filter(in_range(xL,x1,x2) & in_range(yL,y1,y2)) %>%
      dplyr::select(bound_x=xL,bound_y=yL)
    bound_intersections <- bound_vertices %>% dplyr::bind_rows(nearest_on_bounds)
    bound_nearest <- bound_intersections %>%
      dplyr::mutate(dist=sqrt((bound_x-x_loc[i])^2+(bound_y-y_loc[i])^2)) %>%
      dplyr::filter(dist==min(dist)) %>%
      dplyr::distinct()
    dist[i] <- bound_nearest %>% dplyr::pull(dist) %>% min()
    # if (dim(bound_nearest)[1] > 1) {
    #   print(i)
    # }
    # if (i==1) {
    #   loc_nearest <- bound_nearest
    # } else {
    #   loc_nearest <- loc_nearest %>% dplyr::bind_rows(bound_nearest)
    # }
  }

  # if (return_locations) {
  #   dist <- loc_nearest
  # } else {
  #   dist <- loc_nearest$dist
  # }

  return(dist)
}

#' Test if x is in range
#'
#' Test if x is in range [x1,x2]
#' @keywords internal
#' @examples
#' \dontrun{
#' in_range(5,1,4)
#' in_range(5,6,4)
#' in_range(1:5,rep(4,5),c(4,4,5,6,7))
#' }
in_range <- function(x,x1,x2) {
  x_low <- pmin(x1,x2)
  x_high <- pmax(x1,x2)
  in_range <- x >= x_low & x <= x_high
  return(in_range)
}




#' Vertices to edges
#'
#' Convert vertices to edges. It prioritizes quadrangles -- it will close the polygon if
#' there are 4 vertices.
#' @param vertices A data.frame of x, y, and bID
#' @return
#' This function returns bounds object from x, y vertices, with bID 1:4
#' @importFrom magrittr %>%
#' @keywords internal
#' @export
#' @examples
#' vertices <- data.frame(x=c(0,1),y=c(0,1),bID=1:2)
#' get_edges_from_vertices(vertices)
#'
#' vertices <- data.frame(x=c(0,1,1),y=c(0,1,0.5),bID=1:3)
#' get_edges_from_vertices(vertices)
#'
#' vertices <- data.frame(x=c(0,0,1,1),y=c(0,1,1,0),bID=1:4)
#' get_edges_from_vertices(vertices)
get_edges_from_vertices <- function(vertices) {
  edges <- vertices %>% dplyr::rename(x1=x,y1=y) %>%
    dplyr::mutate(x2=dplyr::lead(x1,order_by = bID),
                  y2=dplyr::lead(y1,order_by = bID))
  if (dim(vertices)[1]==4) {
    edges[edges$bID==max(edges$bID),]$x2 <- edges[edges$bID==min(edges$bID),]$x1
    edges[edges$bID==max(edges$bID),]$y2 <- edges[edges$bID==min(edges$bID),]$y1
  }
  return(edges %>% dplyr::filter(!is.na(x2)))
}


#' Get UTM Rectangle (WGS coords)
#'
#' Transform quadrangle (in WGS coordinates) to UTM coordinates,
#' then convert to a rectangle, then transform back to WGS.
#' @param edges_user A data.frame with 4 rows and columns x1, y1, x2, y2, and bID
#' @keywords internal
#' @return
#' Returns a data.frame with 4 rows representing a rectangle. Each row
#' is a segment of the rectangle, and the segments are ordered so that
#' the rectangle is specified as segment 1: p1 --> p2, segment 2: p2 --> p3, etc.
#' @examples
#' \dontrun{
#' edges_user <- data.frame(x1=c(-87.3802501022322,-86.2150051217412,-85.8522401749846,-87.1823783130922),
#'                  y1=c(41.4427263776721,41.8327350621526,41.1455697310095,40.8512155742825),
#'                  bID=c(5,6,7,8),
#'                  x2=c(-86.2150051217412,-85.8522401749846,-87.1823783130922,-87.3802501022322),
#'                  y2=c(41.8327350621526,41.1455697310095,40.8512155742825,41.4427263776721))
#' edges_rect <- get_utm_rectangle(edges_user)
#' }
get_utm_rectangle <- function(edges_user) {

  edges_sf_wgs <- edges_user %>% tidyr::gather(coord,val,dplyr::matches("[xy][12]")) %>%
    tidyr::separate(coord,c("axis","point"),1) %>%
    tidyr::spread(axis,val) %>%
    dplyr::arrange(bID) %>%
    sf::st_as_sf(coords=c("x","y"),crs=4326) %>%
    dplyr::group_by(bID) %>%
    dplyr::summarize() %>% sf::st_cast("LINESTRING")

  edges_center <- suppressWarnings(edges_sf_wgs %>% sf::st_centroid() %>%
                                     sf::st_coordinates() %>% colMeans())
  utm_zone <- longitude_to_utm_zone(edges_center[1])
  edges_sf_utm <- edges_sf_wgs %>% sf::st_transform(utm_zone_to_proj4(utm_zone)) %>%
    prep_bounds_sf()

  edges_rect_pre <- get_rectangle(edges_sf_utm) %>% sf::st_transform(4326) %>%
    dplyr::select(-dplyr::matches("[xy][12]"),-m,-b) %>%
    prep_bounds_sf() %>% sf::st_set_geometry(NULL) %>%
    dplyr::select(bID,x1,y1,x2,y2,dplyr::everything())

  # ggplot() + #geom_sf(data=edges_rect,aes()) +
  #   geom_segment(data=edges_rect,aes(x1,y1,xend=x2,yend=y2)) +
  #   geom_segment(data=edges_user,aes(x1,y1,xend=x2,yend=y2))

  edge_points <- edges_rect_pre %>%
    tidyr::gather(coord,val,dplyr::matches("[xy][12]")) %>%
    tidyr::separate(coord,c("axis","point"),1) %>%
    tidyr::spread(axis,val)
  edges_rect <- edges_rect_pre[1,]
  for (i in 2:4) { # switch [x1,y1] with [x2,y2], if [x1,y1] are repeated
    x_match <- signif(edges_rect$x2[i-1],10) # rounding errors can create problems
    y_match <- signif(edges_rect$y2[i-1],10) # rounding errors can create problems
    edges_left <- edges_rect_pre %>% dplyr::anti_join(edges_rect,by=c("bID"))
    edge_points_left <- edge_points %>% dplyr::filter(bID %in% edges_left$bID)
    next_edge_bID <- edge_points_left %>%
      dplyr::filter(signif(x,10) == x_match,
                    signif(y,10) == y_match) %>%
      purrr::pluck("bID")
    next_edge <- edges_left %>% dplyr::filter(bID==next_edge_bID) # %>% dplyr::slice(1)
    if (signif(next_edge$x1,10)!=x_match | signif(next_edge$y1,10) !=y_match) {
      x2_new <- next_edge$x1
      y2_new <- next_edge$y1
      next_edge$x1 <- next_edge$x2
      next_edge$y1 <- next_edge$y2
      next_edge$x2 <- x2_new
      next_edge$y2 <- y2_new
    }
    edges_rect <- rbind(edges_rect,next_edge)
  }

  # edges_rect$bound_type <- factor("NF",levels=c("NF","CH"))

  return(edges_rect)
}


#' Bounds to SF 2
#'
#' Get sf object from boundaries
#' @param bounds A data.frame containing x1, y1, x2, y2, bID
#' @param crs crs object for sf package
#' @return
#' returns object
#' @keywords internal
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#' bounds <- define_bounds(data.frame(m=c(1,-1,1,-1),b=c(0,2,2,4),bound_type=c("CH","NF","NF","NF")))
#' bounds_sf <- bounds_to_sf(bounds, crs=4326)
#' bounds_sf <- use_anem_function("bounds_to_sf",bounds=bounds,crs=4326)
#' }
bounds_to_sf2 <- function(bounds, crs) {
  bounds_linestring <- bounds %>% dplyr::select(-dplyr::matches("^[mb]$")) %>%
    tidyr::gather(coord,val,dplyr::matches("[xy][12]")) %>%
    tidyr::separate(coord,c("axis","point"),1) %>%
    tidyr::spread(axis,val) %>%
    sf::st_as_sf(coords=c("x","y"),crs=crs) %>%
    dplyr::group_by(bID) %>%
    dplyr::summarize() %>%
    sf::st_cast("LINESTRING")
  bounds_sf <-  bounds %>%
    dplyr::left_join(bounds_linestring ,by=c("bID")) %>% sf::st_as_sf(crs=crs)
  return(bounds_sf)
}

#' Get contour lines
#'
#' Get contour lines, using a wrapper for contourLines
#' @param df A data.frame containing x, y, and z columns. x and y must form a raster,
#'     meaning every x must be represented for each y, and vice versa.
#' @param nlevels An integer containing number of levels to return. Used IFF levels not supplied.
#' @param drop_outer If \code{TRUE}, \code{nlevels+2} will be calculated, dropping the outer 2 values. Used IFF levels not supplied.
#' @param levels A vector of values at which contours should be plotted. If used, nlevels is ignored.
#' @param ... Should contain x, y, and z, and then df will be ignored.
#' @param type Should be either "data.frame" or "sf"
#' @importFrom magrittr %>%
#' @export
#' @examples
#' df <- tidyr::crossing(x=-10:10,y=-10:10) %>% dplyr::mutate(z=x^2)
#' cl <- get_contourlines(df,nlevels=5)
#' unique(cl$level)
#' ggplot() +
#'   geom_raster(data=df,aes(x,y,fill=z)) +
#'   geom_path(data=cl,aes(x,y,group=line))
#'
#' cl <- get_contourlines(df,levels=c(15,20,60))
#' unique(cl$level)
#'
#' df <- tidyr::crossing(x=seq(-5,5,length.out=20),y=seq(-5,5,length.out=20)) %>% dplyr::mutate(z=sqrt(x^2+y^2))
#' cl <- get_contourlines(df,levels=seq(1,120,by=10), type="sf")
#' ggplot() +
#'   geom_raster(data=df,aes(x,y,fill=z)) +
#'   geom_sf(data=cl,aes())
#'
get_contourlines <- function(df = NULL, nlevels = 10, drop_outer = TRUE, levels = NULL, ..., type = "data.frame", crs=4326) {
  params <- list(...)
  if (all(c("x","y","z") %in% names(params))) {
    df$x <- x
    df$y <- y
    df$z <- z
  } else if (!all(c("x","y","z") %in% names(df))) {
    stop("df must contain columns for x, y, and z")
  }
  z_values_check <- df %>% tidyr::complete(x,y) %>% purrr::pluck("z")
  df_error <-  any(is.na(z_values_check) & !is.nan(z_values_check))
  if (df_error) {
    stop("in df, every x, y combination must have a z value.")
  }
  nx <- length(unique(df$x))
  ny <- length(unique(df$y))
  if (nlevels > nx | nlevels > ny) {
    stop("nlevels (",nlevels,") must be less than nx (",nx,") and ny (",ny,").")
  }
  df <- df %>%
    dplyr::arrange(y,x)
  xmat <- df %>% purrr::pluck("x") %>% matrix(ncol=ny)
  ymat <- df %>% purrr::pluck("y") %>% matrix(ncol=ny)
  zmat <- df %>% purrr::pluck("z") %>% matrix(ncol=ny)
  x_seq <- xmat[,1]
  y_seq <- ymat[1,]

  # determine levels
  # z_sorted <- sort(df$z)
  # z_trimmed <- z_sorted[11:(length(z_sorted)-10)]
  if (is.null(levels)) {
    if (drop_outer) {
      levels <- seq(min(df$z,na.rm=TRUE),max(df$z,na.rm=TRUE),length.out=nlevels+2)[2:(nlevels+1)]
    } else {
      levels <- seq(min(df$z,na.rm=TRUE),max(df$z,na.rm=TRUE),length.out=nlevels)
    }
  } else {
    levels <- levels
  }

  # get contour lines
  cl_list <- contourLines(x_seq,y_seq,zmat,levels=levels)
  if (length(cl_list) > 0 ) {
    for (i in 1:length(cl_list)) {
      cl_list[[i]]$line <- i
    }

    # bind contour lines
    cl <- do.call(rbind,lapply(cl_list,function(l) data.frame(x=l$x,y=l$y,level=l$level, line=l$line))) %>%
      dplyr::mutate(i=dplyr::row_number())

    # convert to sf, if requested
    if (type=="sf") {
      cl <- cl %>% sf::st_as_sf(coords=c("x","y"),crs=crs) %>%
        dplyr::arrange(i) %>%
        dplyr::group_by(level,line) %>%
        dplyr::summarize(do_union=FALSE) %>%
        sf::st_cast("LINESTRING")
    }
  } else {
    warning(paste0("No contours found. Level range: (",min(level),",",max(level),"). ",
                   "z range: (",min(df$z,na.rm=TRUE),",",max(df$z,na.rm=TRUE),")"))
    cl <- NULL
  }
  return(cl)
}

#' Bounds to polygon
#'
#' Convert bounds as sf object to polygon
#' @param bounds_sf An sf object containing 4 line segments that meet at the ends to
#'   form a polygon
#' @keywords internal
bounds_sf_to_polygon <- function(bounds_sf) {
  bounds_sf %>% dplyr::mutate(L1=row_number())

  bounds_crs <- bounds_sf %>% sf::st_crs()

  bounds_pts <- bounds_sf %>% sf::st_coordinates() %>%
    signif(10) %>% as.data.frame() %>%
    dplyr::rename(x=X,y=Y)

  p <- bounds_pts[1,]
  for (i in 2:4) {
    next_pt_coords <- bounds_pts %>% dplyr::filter(L1 %in% p$L1,!((x %in% p$x) & (y %in% p$y)))
    next_pt <- bounds_pts %>% dplyr::filter(!(L1 %in% p$L1),x %in% next_pt_coords$x, y %in% next_pt_coords$y)
    p[i,] <- next_pt
  }

  bound_polygon <- p %>% sf::st_as_sf(coords=c("x","y"),crs=bounds_crs) %>%
    dplyr::group_by() %>%
    dplyr::summarize(do_union=FALSE) %>%
    sf::st_cast("POLYGON")

  return(bound_polygon)
}

#' Get perpendicular line
#'
#' Get perpendicular in from slope and point
#' @param m Slope of line
#' @param x Coordinate through which perpendicular line should pass
#' @param y Coordinate through which perpendicular line should pass
#' @return
#' Returns a list containing $m and $b, the slope and intercept of a perpendicular
#' line that passes through x, y.
#' @keywords internal
#' @examples
#' \dontrun{
#' get_perpendicular_line(Inf,2,3)
#' get_perpendicular_line(0,2,3)
#' get_perpendicular_line(1/2,2,2)
#' get_perpendicular_line(-2,2,2)
#' }
get_perpendicular_line <- function(m,x,y) {
  if (abs(m==Inf)) {
    m_perp <- 0
    b_perp <- y
  } else if (m==0) {
    m_perp <- Inf
    b_perp <- x
  } else {
    m_perp <- -1/m
    b_perp <- y - m_perp*x
  }
  return(list(m=m_perp,b=b_perp))
}

#' Check point is in bounds
#'
#' Check to see if one or more points falls within aquifer boundaries.
#' @return
#' Function returns \code{TRUE} if the point is inside aquifer boundaries or on the boundary. If the point is outside
#'the boundaries, returns \code{FALSE}.
#' @keywords internal
#' @examples
#' \dontrun{
#' aquifer <- aquifer_unconfined_example
#' x <- 100
#' y <- 100
#' x <- seq(0,100,by=50)
#' y <- seq(0,100,by=50)
#' check_point_in_aquifer(200,200,aquifer)
#' check_point_in_aquifer(-200,0,aquifer)
#' check_point_in_aquifer(seq(0,100,by=50),seq(0,100,by=50),aquifer)
#' check_point_in_aquifer(seq(0,1000,by=500),seq(0,100,by=500),aquifer)
#' x <- rep(500,3)
#' y <- seq(0,1000,by=500)
#' check_point_in_aquifer(x,yseq(0,1000,by=500),aquifer)
#' }
check_point_in_aquifer <- function(x,y,aquifer) {
  if (any(grepl("sf",class(aquifer$bounds)))) {
    bounds <- aquifer$bounds %>% sf::st_set_geometry(NULL)
  } else {
    bounds <- aquifer$bounds
  }
  b_a <- bounds %>% dplyr::filter(m==bounds$m[1])
  side_a <- sapply(split(b_a,1:nrow(b_a)),check_point_side_of_line,x=x,y=y)

  b_b <- bounds %>% dplyr::anti_join(b_a,by=names(b_a))
  side_b <- sapply(split(b_b,1:nrow(b_b)),check_point_side_of_line,x=x,y=y)

  if (length(x) == 1) {
    side_a <- matrix(side_a,nrow=1)
    side_b <- matrix(side_b,nrow=1)
  }

  inside_a <- side_a[,1] != side_a[,2] | side_a[,1] == 0 & side_a[,2] == 0
  inside_b <- side_b[,1] != side_b[,2] | side_b[,1] == 0 & side_b[,2] == 0

  inside_bounds <- inside_a & inside_b

  return(inside_bounds)
}

#' Check point is in bounds
#'
#' @param x x coordinates
#' @param y y coordinates
#' @param line list containing m and m
#' @keywords internal
#' @return
#' Returns +1 for any point above the line, -1 for any point below the line, and 0 for a point on the line. If
#' the line is vertical (m=Inf), returns +1 for points to the right of the line and -1 for points to the left of the line.
#' @examples
#' \dontrun{
#' x <- 100
#' y <- 100
#' line <- list(m=Inf,b=2)
#' check_point_side_of_line(list(m=Inf,b=2),0,0)
#' check_point_side_of_line(list(m=Inf,b=2),2,0)
#' check_point_side_of_line(list(m=Inf,b=2),3,0)
#' check_point_side_of_line(list(m=1,b=2),0,0)
#' check_point_side_of_line(list(m=1,b=2),0,2)
#' check_point_side_of_line(list(m=1,b=2),0,3)
#' }
check_point_side_of_line <- function(line,x,y) {
  if (line$m != Inf) {
    return(sign(y - (line$m * x + line$b)))
  } else {
    return(sign(x - line$b))
  }
}
