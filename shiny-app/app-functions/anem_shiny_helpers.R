# Helpfer functions for shiny

#' Interpret map click
#'
#' Interpret leaflet map click (not on a marker). Translate to a new
#' bound vertex or a new well.
#' @param usermode From input$usermode, which should be one of "aquifer", "bounds", "wells"
#' @param map_click From input$map_click, or input$mapname_click,
#'     which is the output from the leaflet map
#' @param mapclicks A reactive object, which is a list containing text,
#'     bound_vertices, and well_locations
#' @param ... Required parameters depending on clickOperation
#' @result
#' if clickOperation is new_well, then \code{...} must contain \code{well_input}
interpret_map_click <- function(mapClick, clickOperation, mapclicks, ...) {
  inputs <- list(...)
  x <- mapClick$lng
  y <- mapClick$lat
  if (clickOperation == "aquifer_vertex") {
    newid <- max(c(mapclicks$bound_vertices$bID,0),na.rm=TRUE) + 1
    # Clear bound vertices if a click occurs after 4 vertices already set
    if (dim(mapclicks$bound_vertices)[1] >= 4) {
      mapclicks$bound_vertices <- mapclicks$bound_vertices[FALSE,]
    }
    mapclicks$bound_vertices <- rbind(mapclicks$bound_vertices,
                                      data.frame(x=x,y=y,bID=newid))
    # if (dim(mapclicks$bound_vertices)[1] >= 4) {
    #   mapclicks$bounds_rectangular <-
    # }
  } else if (clickOperation == "new_well") {
    newid <- max(c(mapclicks$well_locations$wID,0),na.rm=TRUE) + 1
    mapclicks$well_locations <- rbind(mapclicks$well_locations %>% dplyr::mutate(selected=FALSE),
                                      data.frame(Q=inputs$well_input$Q,R=inputs$well_input$R,diam=inputs$well_input$diam,
                                                 x=x,y=y,wID=newid,selected=TRUE))
  }
  return(mapclicks)
}


# edges_user <- data.frame(x1=c(-87.3802501022322,-86.2150051217412,-85.8522401749846,-87.1823783130922),
#                  y1=c(41.4427263776721,41.8327350621526,41.1455697310095,40.8512155742825),
#                  bID=c(5,6,7,8),
#                  x2=c(-86.2150051217412,-85.8522401749846,-87.1823783130922,-87.3802501022322),
#                  y2=c(41.8327350621526,41.1455697310095,40.8512155742825,41.4427263776721))
# edges_rect <- get_utm_rectangle(edges_user)
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

  edges_rect$bound_type <- factor("NF",levels=c("NF","CH"))

  return(edges_rect)
}

