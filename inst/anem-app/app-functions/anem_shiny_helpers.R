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
    new_row <- data.frame(x=x,y=y)
    # only add the point if it is different from existing points
    if (nrow(base::merge(new_row,mapclicks$bound_vertices))==0) {
      newid <- max(c(mapclicks$bound_vertices$bID,0),na.rm=TRUE) + 1
      # Clear bound vertices if a click occurs after 4 vertices already set
      if (dim(mapclicks$bound_vertices)[1] >= 4) {
        mapclicks$bound_vertices <- mapclicks$bound_vertices[FALSE,]
      }
      mapclicks$bound_vertices <- rbind(mapclicks$bound_vertices,
                                        data.frame(x=x,y=y,bID=newid))
    }
  } else if (clickOperation == "recharge_vertex") {
    new_row <- data.frame(x=x,y=y)
    # only add the point if it is different from existing points
    if (nrow(base::merge(new_row,mapclicks$recharge_vertices))==0) {
      newid <- max(c(mapclicks$recharge_vertices$rID,0),na.rm=TRUE) + 1
      # Clear bound vertices if a click occurs after 4 vertices already set
      if (dim(mapclicks$recharge_vertices)[1] >= 2) {
        mapclicks$recharge_vertices <- mapclicks$recharge_vertices[FALSE,]
      }
      mapclicks$recharge_vertices <- rbind(mapclicks$recharge_vertices,
                                        data.frame(x=x,y=y,rID=newid))
    }
  } else if (clickOperation == "new_well") {
    newid <- max(c(mapclicks$well_locations$wID,0),na.rm=TRUE) + 1
    mapclicks$well_locations <- rbind(mapclicks$well_locations %>% dplyr::mutate(selected=FALSE),
                                      data.frame(Q=inputs$well_input$Q,R=inputs$well_input$R,diam=inputs$well_input$diam,
                                                 Group=inputs$well_input$group,Weight=inputs$well_input$weight,
                                                 x=x,y=y,wID=newid,selected=TRUE,stringsAsFactors = FALSE)) %>%
      dplyr::mutate_at(vars(Q,R,diam,Weight),as.numeric)
  } else if (clickOperation == "new_particle") {
    newpid <- max(c(mapclicks$particle_locations$pID,0),na.rm=TRUE) + 1
    mapclicks$particle_locations <- rbind(mapclicks$particle_locations %>% dplyr::mutate(selected=FALSE),
                                      data.frame(pID=newpid,x=x,y=y,
                                                 selected=TRUE,stringsAsFactors = FALSE))
  }
  return(mapclicks)
}
