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
interpret_map_click <- function(clickedMarker, usermode, mapclicks, well_input) {
  x <- clickedMarker$lng
  y <- clickedMarker$lat
  if (usermode == "aquifer") {
    newid <- max(c(mapclicks$bound_vertices$id,0),na.rm=TRUE) + 1
    # Clear bound vertices if a click occurs after 4 vertices already set
    if (dim(mapclicks$bound_vertices)[1] >= 4) {
      mapclicks$bound_vertices <- mapclicks$bound_vertices[FALSE,]
    }
    mapclicks$bound_vertices <- rbind(mapclicks$bound_vertices,
                                      data.frame(x=x,y=y,id=newid))
  } else if (usermode == "wells") {
    newid <- max(c(mapclicks$well_locations$id,0),na.rm=TRUE) + 1
    mapclicks$well_locations <- rbind(mapclicks$well_locations,
                                      data.frame(Q=well_input$Q,R=well_input$R,diam=well_input$diam,
                                                 x=x,y=y,id=newid))
  }
  return(mapclicks)
}

#' Vertices to edges
#'
#' Convert vertices to edges. It prioritizes quadrangles -- it will close the polygon if
#' there are 4 vertices.
#' @param df
#' @return
#' This function returns bounds object from x, y vertices, with id 1:4
#' @importFrom magrittr %>%
#' @examples
#' vertices <- data.frame(x=c(0,1),y=c(0,1),id=1:2)
#' get_edges_from_vertices(vertices)
#'
#' vertices <- data.frame(x=c(0,1,1),y=c(0,1,0.5),id=1:3)
#' get_edges_from_vertices(vertices)
#'
#' vertices <- data.frame(x=c(0,0,1,1),y=c(0,1,1,0),id=1:4)
#' get_edges_from_vertices(vertices)
get_edges_from_vertices <- function(vertices) {
  edges <- vertices %>% dplyr::rename(x1=x,y1=y) %>%
    dplyr::mutate(x2=dplyr::lead(x1,order_by = id),
                  y2=dplyr::lead(y1,order_by = id))
  if (dim(vertices)[1]==4) {
    edges[edges$id==max(edges$id),]$x2 <- edges[edges$id==min(edges$id),]$x1
    edges[edges$id==max(edges$id),]$y2 <- edges[edges$id==min(edges$id),]$y1
  }
  return(edges %>% dplyr::filter(!is.na(x2)))
}
