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
#' @examples
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
get_rectangle <- function(bounds) {
  is_sf <- max(grepl("sf",class(bounds)))

  if (is.null(bounds$bID)) {
    bounds <- bounds %>%
      dplyr::mutate(bID=dplyr::row_number())
  }

  # get vertices (and remove extraneous properties, including sf)
  quad_vertices_full <- get_quad_vertices(bounds)

  # get midpoints of bounds
  midpoints <- quad_vertices_full %>% dplyr::group_by(bID) %>%
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
    dplyr::group_by(bID)
  quad_vertices_final_wide <- quad_vertices_final %>%
    dplyr::mutate(pt_num=dplyr::row_number()) %>% dplyr::group_by() %>%
    tidyr::gather(var,val,x,y) %>%
    tidyr::unite("pt",var,pt_num,sep = "") %>%
    tidyr::spread(pt,val)

  bounds_rectangular <- bounds_rectangular %>% dplyr::left_join(quad_vertices_final_wide,by="bID")

  if(is_sf) {
    quad_vertices_final_sf <- quad_vertices_final %>%
      sf::st_as_sf(coords=c("x","y")) %>%
      sf::st_set_crs(sf::st_crs(bounds))
    quad_vertices_final_linestring <- quad_vertices_final_sf %>% group_by(bID) %>%
      dplyr::summarize(geometry=sf::st_union(geometry)) %>%
      sf::st_cast("LINESTRING",union=TRUE) %>%
      sf::st_set_crs(sf::st_crs(bounds))
    bounds_rectangular <- quad_vertices_final_linestring %>%
      left_join(bounds_rectangular,by="bID")
  }

  return(bounds_rectangular)
}

#' Get bounds as sf object
#'
#' Convert bounds object with bID, x1, y1, x2, y2 to sf object
#' @examples
#' bounds <- data.frame(bID=1,x1=1,y1=1,x2=3,y2=3)
#' bounds_to_sf(bounds,crs=4326)
bounds_to_sf <- function(bounds,crs) {
  bounds_geometry <- bounds %>% tidyr::gather(coord,val,x1,y1,x2,y2) %>%
    dplyr::mutate(axis=substr(coord,1,1),
                  pt_num=substr(coord,2,2)) %>%
    dplyr::select(-coord) %>% tidyr::spread(axis,val) %>%
    sf::st_as_sf(coords=c("x","y")) %>% sf::st_set_crs(crs) %>%
    dplyr::group_by(bID) %>%
    dplyr::summarize() %>% sf::st_cast("LINESTRING")
  bounds_with_sf <- bounds_geometry %>% dplyr::left_join(bounds,by="bID") %>%
    dplyr::select(!! names(bounds),everything())
  return(bounds_with_sf)
}

#' Get vertices of a quadrangle
#'
#' Get the 4 vertices corresponding to quadrangle of 4 lines
#' @param bounds a data.frame containing 4 lines (rows) defined by bID and x1, y1, x2, y2, or m and b
#' @importFrom magrittr %>%
#' @return Returns a data.frame vertices for each of the 4 bounds. The bounds
#' are given an id, and there is one row in the output which identifies the opposide bound
#' (with which there is no intersection).
#' @examples
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

#' Generate outline of circle
#'
#' @param circle A \code{data.frame} with $x, $y, and $r columns. $id optional
#' @importFrom magrittr %>%
gen_circleFun <- function(circle, npoints = 100){
  center <- c(circle$x,circle$y)
  r <- circle$r
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
gen_circ <- function(x,y,r) {
  return(list(x=x,y=y,r=r))
}

#' Generate an object that acts like a circle
#'
#' @param df data.frame with columns x, y, r
#' @export
#' @examples
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
get_quad_vertices_wide <- function(bounds) {
  bounds <- get_quad_vertices(bounds) %>% filter(!is.na(x)) %>% group_by(bID) %>%
    mutate(num=rank(intersection_bID)) %>% select(-intersection_bID) %>%
    gather(axis,val,x,y) %>% unite(pt,axis,num) %>% spread(pt,val)
  return(bounds)
}

#' Get slope and intercept
#'
#' Get slope and intercept of a line defined by x1, y1, x2, y2
#' @examples
#' get_slope_intercept(0,0,1,2)
#' get_slope_intercept(0,-0,2,-1)
#' get_slope_intercept(0,0,0,3)
#' get_slope_intercept(0,2,3,2)
#' get_slope_intercept(1,1,m=-1)
#' get_slope_intercept(1,1,m=Inf)
get_slope_intercept <- function(x1,y1,x2=NULL,y2=NULL,m=NULL) {
  if (is.null(m)) {
    m <- dplyr::if_else(x2==x1, Inf, (y2-y1)/(x2-x1))
    b <- dplyr::if_else(x2==x1, x1, y1 - m*x1)
  } else {
    b <- dplyr::if_else(abs(m)==Inf, x1, y1 - m*x1)
  }
  return(list(m=m,b=b))
}

#' Print data frame for re-entry
#' @examples
#' df <- data.frame(a=1:4,b=11:14,c=c("a","b","c","q"))
#' print_data_frame_for_entry(df)
print_data_frame_for_entry <- function(df) {
  df_names <- names(df)
  N <- length(df_names)
  cat("df <- data.frame(")
  for (i in 1:N) {
    char_end <- ifelse(i!=N,",",")")
    cat(paste0(df_names[i],"=",paste(df[,i],collapse=","),"",char_end,"\n"))
  }

}
