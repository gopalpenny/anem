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
interpret_map_click <- function(mapClick, clickOperation, mapclicks, well_input) {
  x <- mapClick$lng
  y <- mapClick$lat
  if (clickOperation == "aquifer_vertex") {
    newid <- max(c(mapclicks$bound_vertices$id,0),na.rm=TRUE) + 1
    # Clear bound vertices if a click occurs after 4 vertices already set
    if (dim(mapclicks$bound_vertices)[1] >= 4) {
      mapclicks$bound_vertices <- mapclicks$bound_vertices[FALSE,]
    }
    mapclicks$bound_vertices <- rbind(mapclicks$bound_vertices,
                                      data.frame(x=x,y=y,id=newid))
  } else if (clickOperation == "new_well") {
    newid <- max(c(mapclicks$well_locations$id,0),na.rm=TRUE) + 1
    mapclicks$well_locations <- rbind(mapclicks$well_locations %>% dplyr::mutate(selected=FALSE),
                                      data.frame(Q=well_input$Q,R=well_input$R,diam=well_input$diam,
                                                 x=x,y=y,id=newid,selected=TRUE))
  }
  return(mapclicks)
}


