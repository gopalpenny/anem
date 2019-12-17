# anem_ui.R
library(leaflet)
library(anem)
library(DT)
library(ggplot2)
library(shinycssloaders)
library(raster)
library(mapview)

source("app-functions/anem_shiny_helpers.R")

wellPal <- colorFactor(palette = c("darkgreen","green"), domain = c(FALSE, TRUE))
boundPal <- colorFactor(palette = c("blue","black"), domain = c("NF", "CH"))

ui <- fluidPage(
  h4("anem ui"),
  tabsetPanel(id="maintabs",
    type="pills",
    tabPanel(
      "Prepare scenario",value="prepare",fluid=TRUE,
      fluidRow(
        # Prepare scenario
        hr(),
        column(
          4,
          # verbatimTextOutput("utm_zone"),
          radioButtons("usermode","Entry mode",
                       c("Define aquifer" = "aquifer",
                         # "Set aquifer bounds" = "bounds",
                         "Add or modify wells" = "wells")),
          conditionalPanel(
            condition = "input.usermode == 'aquifer'",
            # selectInput("aquifer_input","Edit aquifer",
            #             c("Aquifer properties"="properties",
            #               "Aquifer boundaries"="boundaries")),

            tabsetPanel(
              type="pills",
              tabPanel(
                "Properties",fluid=TRUE,
                # conditionalPanel(
                #   condition = "input.usermode == 'aquifer' & input.aquifer_input == 'properties'",
                # h5("Aquifer type"),
                radioButtons("aquifer_type", "Aquifer type", #"Aquifer type",
                             c("Confined" = "confined","Unconfined" = "unconfined")),
                numericInput("Ksat", "Saturated hydraulic conductivity, m/s^2",value = 0.001),
                numericInput("h0", "Undisturbed hydraulic head, m",value = 50),
                conditionalPanel(
                  condition = "input.aquifer_type == 'confined'",
                  numericInput("z0", "Aquifer thickness, m",10)
                )
              ),
              tabPanel(
                "Boundaries",fluid=TRUE,
                #     "Prepare scenario",fluid=TRUE,
                # conditionalPanel(
                #   condition="input.usermode == 'aquifer' & input.aquifer_input == 'boundaries'",
                # h4("Aquifer "),
                p("Click 4 points to add rectangular aquifer boundaries."), #bound_type can be \"NF\" (no flow) or \"CH\" (constant head)"),
                fluidRow(
                  # column(6,dataTableOutput("edgetable")),
                  column(5,h4("Bound 1:")),
                  column(7,selectInput("b1_type",NULL,choices = c("No flow"="NF","Constant head"="CH"),selected = "No flow"))
                ),
                fluidRow(
                  # column(6,dataTableOutput("edgetable")),
                  column(5,h4("Bound 2:")),
                  column(7,selectInput("b2_type",NULL,choices = c("No flow"="NF","Constant head"="CH"),selected = "No flow"))
                ),
                fluidRow(
                  # column(6,dataTableOutput("edgetable")),
                  column(5,h4("Bound 3:")),
                  column(7,selectInput("b3_type",NULL,choices = c("No flow"="NF","Constant head"="CH"),selected = "No flow"))
                ),
                fluidRow(
                  # column(6,dataTableOutput("edgetable")),
                  column(5,h4("Bound 4:")),
                  column(7,selectInput("b4_type",NULL,choices = c("No flow"="NF","Constant head"="CH"),selected = "No flow"))
                )#,verbatimTextOutput("boundtypes")
              )
            )
          ),
          conditionalPanel(
            condition = "input.usermode == 'bounds' | input.usermode == 'wells'",
            # h5("Instructions"),
            textOutput("prepinstr"),
            h4("New well:"),
            fluidRow(
              column(4,numericInput("Q","Q",0.1)),
              column(4,numericInput("R","R",9000)),
              column(4,numericInput("diam","diam",1))
            ),
            h4("Edit well:"),
            fluidRow(
              column(6,actionButton("deleteWell","Delete well"),offset=3)
            )
          )
        ),
        # Prepare map
        column(8,
               fluidRow(
                 column(4,h3(textOutput("prepmaptitle"))),
                 column(4,actionButton("resetMap","Reset map",style='padding:4px; font-size:80%'),offset=4)
               ),
               leafletOutput("prepmap",height=420)
        ),
      ),
      fluidRow(
        hr(),
        # h4(textOutput("usermode_elements")),
        h4("Wells (double click to edit)"),
        conditionalPanel(
          condition="input.usermode == 'wells'",
                    dataTableOutput("welltable")),
        #   hr(),
        verbatimTextOutput("aquifer"),
        verbatimTextOutput("bounds"),
        verbatimTextOutput("wells")
      )

    ),
    tabPanel(
      "View results",value="results",fluid=TRUE,
      fluidRow(
        hr(),
        column(4,
               checkboxInput("linkmaps","Link maps",TRUE),
               checkboxInput("include_gridded_head","Gridded head, m",TRUE)
        ),
        column(8,
               fluidRow(
                 column(4,h3("Results")),
                 column(4,actionButton("donothing","Does nothing",style='padding:4px; font-size:80%'),offset=4)
               ),
               leafletOutput("resultsmap",height=420) %>% withSpinner(),
               plotOutput("results")
        )
      )

    )
  )

)

server <- function(input, output) {

  # Check when to update results
  updateResults <- reactiveValues(
    next_view=TRUE,
    update_now=TRUE
  )

  # Initialize reactive values
  mapclicks <- reactiveValues(
    bound_vertices=data.frame(bID=integer(),#bound_type=factor(levels=c("NF","CH")),
                              x=numeric(),y=numeric()),
    well_locations=data.frame(Q=numeric(),R=numeric(),diam=numeric(),
                              x=numeric(),y=numeric(),wID=integer(),selected=logical())
  )

  bounds <- reactiveValues(
    edges_user = NULL,
    edges_rectangular = NULL,
    bounds_sf = NULL
  )

  bound_types <- reactive({
    c(input$b1_type,input$b2_type,input$b3_type,input$b4_type)
  })

  aquifer <- reactive({define_aquifer(
    aquifer_type=input$aquifer_type,
    h0=input$h0,
    Ksat=input$Ksat,
    z0=input$z0,
    bounds=bounds$bounds_sf)})

  utm_zone <- reactive({
    if(is.null(bounds$bounds_sf)) {
      zone <- NULL
    } else if (nrow(bounds$bounds_sf) == 0) {
      zone <- NULL
    } else {
      centroid <- bounds$bounds_sf %>% sf::st_coordinates() %>% colMeans()
      zone <- anem::longitude_to_utm_zone(centroid[1])
    }
    zone
  })

  output$utm_zone <- renderPrint({
    print(utm_zone())
  })

  observeEvent(bounds$bounds_sf,{
    updateResults$next_view <- TRUE
  })
  observeEvent(mapclicks$well_locations,{
    updateResults$next_view <- TRUE
  })

  observeEvent(bound_types(),{
    # print(bounds$bounds_sf)
    if (!is.null(bounds$bounds_sf)) {
      bounds$bounds_sf <- bounds$bounds_sf %>% dplyr::mutate(bound_type=bound_types())
      # print(bounds$bounds_sf)
      leafletProxy("prepmap") %>%
        clearGroup("bounds_rectangular") %>%
        addPolylines(color = ~boundPal(bound_type), group = "bounds_rectangular",
                     fillOpacity = 0, opacity = 1, weight = 4,
                     data=bounds$bounds_sf)
    }
  })
  output$boundtypes <- renderPrint({print(bound_types())})

  output$prepinstr <- renderText({
    # paste("usermode:",input$usermode)
    switch(input$usermode,
           "aquifer" = "Set aquifer properties.",
           "bounds" = "Click 4 vertices on the map to define the aquifer.",
           "wells" = "Click a well to edit, or click an empty space to add a well.")
  })
  output$usermode_elements <- renderText({
    # paste("usermode:",input$usermode)
    switch(input$usermode,
           "aquifer" = "Aquifer boundaries (double click to edit)",
           "wells" = "Wells (double click to edit)")
  })

  output$prepmaptitle <- renderText({
    # paste("usermode:",input$usermode)
    switch(input$usermode,
           "aquifer" = "Define aquifer",
           "wells" = "Define wells")
  })

  output$prepmap <- renderLeaflet({
    leaflet() %>%
      addTiles(options = providerTileOptions(noWrap = TRUE), group="Map") %>%
      leafem::addMouseCoordinates() %>%
      addProviderTiles("Esri.WorldImagery", group="Satellite") %>%
      addScaleBar(position = c("bottomright"), options = scaleBarOptions()) %>%
      addLayersControl(baseGroups = c("Map","Satellite"),#overlayGroups = c("Red","Blue") ,
                       options = layersControlOptions(collapsed = FALSE)) %>%
      setView(lng = -86.252, lat = 41.676, zoom=10)
  })
  output$resultsmap <- renderLeaflet({
    leaflet() %>%
      addTiles(options = providerTileOptions(noWrap = TRUE), group="Map") %>%
      leafem::addMouseCoordinates() %>%
      addProviderTiles("Esri.WorldImagery", group="Satellite") %>%
      addScaleBar(position = c("bottomright"), options = scaleBarOptions()) %>%
      addLayersControl(baseGroups = c("Map","Satellite"),#overlayGroups = c("Red","Blue") ,
                       options = layersControlOptions(collapsed = FALSE)) %>%
      setView(lng = -86.252, lat = 41.676, zoom=10)
  })

  # Map click (new well or aquifer vertex)
  observeEvent(input$prepmap_click,{
    prepmapClick <- input$prepmap_click
    if (!is.null(input$prepmap_marker_click)) {
      markerClick <- input$prepmap_marker_click
    } else {
      markerClick <- list(lat=Inf,lng=Inf)
    }
    prepmap_marker_equal <- identical(prepmapClick[c("lng","lat")],markerClick[c("lng","lat")])
    clickType <- dplyr::case_when(
      prepmap_marker_equal ~ "marker",
      !prepmap_marker_equal ~ "map")
    clickOperation <- dplyr::case_when(
      input$usermode=="aquifer" ~ "aquifer_vertex",
      input$usermode=="wells" & clickType=="map" ~ "new_well",
      input$usermode=="wells" & clickType=="marker" ~ "edit_well"
    )
    well_input <- list(Q=input$Q,R=input$R,diam=input$diam)
    mapclicks <- interpret_map_click(prepmapClick,clickOperation,mapclicks,well_input=well_input)
    if (input$usermode == "aquifer") {
      bounds$edges_user <- get_edges_from_vertices(mapclicks$bound_vertices)
      leafletProxy("prepmap",data=mapclicks$bound_vertices) %>%
        clearGroup("boundvertices") %>% leaflet::clearGroup("bounds_rectangular") %>%
        addCircleMarkers(~x, ~y, color = "black", group = "boundvertices") %>%
        addPolygons(~x, ~y, color = "black", dashArray = "10 10", opacity = 0.3, weight = 2,
                    layerId = "boundedges",fillOpacity = 0)
      if (dim(bounds$edges_user)[1]==4) {
        print("1")
        bounds$edges_rectangular <-
          use_anem_function("get_utm_rectangle",
                            edges_user=bounds$edges_user) %>%
          dplyr::mutate(bound_type=bound_types()) %>%
          dplyr::select(bID,bound_type,dplyr::everything()) %>%
          dplyr::arrange(bID)
        print("2")
        bounds$bounds_sf <- use_anem_function("bounds_to_sf",bounds$edges_rectangular,crs=4326)
        print("3")
        # print(bounds$bounds_sf)
        leafletProxy("prepmap") %>%
          clearGroup("boundvertices") %>%
          addPolylines(color = ~boundPal(bound_type), group = "bounds_rectangular",
                      fillOpacity = 0, opacity = 1, weight = 4,
                      data=bounds$bounds_sf)
      }
    } else if (clickOperation == "new_well") {
      leafletProxy("prepmap") %>%
        clearGroup("wells") %>%
        addCircleMarkers(~x, ~y, color = ~wellPal(selected), group = "wells",
                         data=mapclicks$well_locations)
    } else if (clickOperation == "edit_well") {
      mapclicks$well_locations <- mapclicks$well_locations %>%
        dplyr::mutate(dist=sqrt((x-markerClick$lng)^2 + (y - markerClick$lat)^2),
                      selected=dist==min(dist)) %>%
        dplyr::select(-dist)
      # dist <- which.min(sqrt((mapclicks$well_locations$lng - markerClick$lng)^2 +
      #                      (mapclicks$well_locations$lat - markerClick$lat)^2))
      # closest_marker <-
      # mapclicks$well_locations[closest_well,"selected"] <- TRUE
      # print("mapclicks$well_locations")
      # print(mapclicks$well_locations)
      leafletProxy("prepmap") %>%
        clearGroup("wells") %>%
        addCircleMarkers(~x, ~y, color = ~wellPal(selected), group = "wells",
                         data=mapclicks$well_locations)
    }
  })

  # # Map marker click (select well)
  # observeEvent(input$map_marker_click,{
  #   markerClick <- input$map_marker_click
  #   output$current_well <- renderPrint({print(markerClick)})
  # })

  observeEvent(input$deleteWell,{
    mapclicks$well_locations <- mapclicks$well_locations %>%
      dplyr::filter(!selected)
    leafletProxy("prepmap") %>%
      clearGroup("wells") %>%
      addCircleMarkers(~x, ~y, color = ~wellPal(selected), group = "wells",
                       data=mapclicks$well_locations)
  })

  observeEvent(input$resetMap,{
    mapclicks$bound_vertices <- data.frame(x=numeric(),y=numeric(),bID=integer())
    mapclicks$well_locations <- data.frame(Q=numeric(),R=numeric(),diam=numeric(),
                              x=numeric(),y=numeric(),wID=integer(),selected=logical())
    leafletProxy("prepmap") %>%
      clearShapes() %>% clearMarkers()
  })

  output$welltable <- renderDataTable(
    datatable(mapclicks$well_locations,
              editable=T,rownames=F,
              options = list(searching=FALSE,
                             # formatNumber= function(x) format(x,nsmall=3),
                             lengthChange=FALSE,
                             autoWidth = TRUE,
                             columnDefs = list(list(width = '200px', targets = "_all")))
              ) %>%
      formatStyle('selected',target='row',
                  backgroundColor = styleEqual(c(FALSE,TRUE),c('white','lightgreen'))))


  # output$wellDT <- renderDT(mapclicks$well_locations, selection = 'none', rownames = T, editable = T)
  #
  proxy_welltable <- dataTableProxy('welltable')
  # proxy_edgetable <- dataTableProxy('edgetable')

  observeEvent(input$welltable_cell_edit, {
    info = input$welltable_cell_edit
    # str(info)
    i = info$row
    j = info$col + 1  # column index offset by 1
    v = info$value
    mapclicks$well_locations[i, j] <<- DT::coerceValue(v, mapclicks$well_locations[i, j])
    replaceData(proxy_welltable, mapclicks$well_locations, resetPaging = FALSE, rownames = F)
    if (j %in% c(4,5)) { # update map if x or y change
      leafletProxy("prepmap") %>%
        clearGroup("wells") %>%
        addCircleMarkers(~x, ~y, color = ~wellPal(selected), group = "wells",
                         data=mapclicks$well_locations)
    }
  })

  observeEvent(input$prepmap_bounds,{
    if (input$linkmaps) {
      # print("prepmapbounds")
      prepmapbounds <- input$prepmap_bounds
      leafletProxy("resultsmap") %>%
        fitBounds(prepmapbounds$west,prepmapbounds$south,
                  prepmapbounds$east,prepmapbounds$north,
                  options(animate=FALSE,duration=0))
    }
  })
  observeEvent(input$resultsmap_bounds,{
    if (input$linkmaps) {
      resultsmap_bounds <- input$resultsmap_bounds
      leafletProxy("prepmap") %>%
        fitBounds(resultsmap_bounds$west,resultsmap_bounds$south,
                  resultsmap_bounds$east,resultsmap_bounds$north,
                  options(animate=FALSE,duration=0))
    }
  })

  observeEvent(input$maintabs,{
    # print(input$maintabs)
    if (input$maintabs == "results" & updateResults$next_view) {
      updateResults$update_now <- TRUE
    }
  })

  observeEvent(updateResults$update_now,{
    print('updating now')
    print('updateResults$update_now')
    print('nrow')
    print(nrow(mapclicks$well_locations))
    if (nrow(mapclicks$well_locations) > 0) {
      wells_wgs <- mapclicks$well_locations %>%
        sf::st_as_sf(coords=c("x","y"),crs=4326)
    } else {
      wells_wgs <- NULL
    }
    print("2")
    bounds_utm <- NULL
    # if there are bounds, map them and get proj4string
    if(!is.null(bounds$bounds_sf)) {
      proj4string_scenario <- anem::utm_zone_to_proj4(utm_zone())
      bounds_utm <- bounds$bounds_sf %>%
        dplyr::select(-dplyr::matches('^[mb]$'),-dplyr::matches("[xy][12]")) %>%
        sf::st_transform(anem::utm_zone_to_proj4(utm_zone()))
      print("2.1")
      aquifer_utm <- aquifer()
      print("2.2")
      # saveRDS(bounds_utm,"app-data/bounds_utm.rds")
      print("2.3")
      aquifer_utm$bounds <- define_bounds(bounds_utm)
      print("3")
      # print(bounds$bounds_sf)
      leafletProxy("resultsmap") %>%
        clearGroup("bounds_rectangular") %>%
        clearControls() %>%
        addPolylines(color = ~boundPal(bound_type), group = "bounds_rectangular",
                     fillOpacity = 0, opacity = 1, weight = 4,
                     data=bounds$bounds_sf)
      print('4')
      # if no bounds, get proj4string from wells
    } else {
      print('3x')
      if(!is.null(wells_wgs)) {
        aquifer_utm <- aquifer()
        centroid <- wells_wgs %>% sf::st_coordinates() %>% colMeans()
        zone <- anem::longitude_to_utm_zone(centroid[1])
        proj4string_scenario <- anem::utm_zone_to_proj4(zone)
      }
    }
    if(!is.null(wells_wgs)) {
      wells_utm <- wells_wgs %>%
        sf::st_transform(crs=proj4string_scenario) %>%
        define_wells() %>%
        generate_image_wells(aquifer_utm)
      print("n wells")
      print(wells_utm)
      leafletProxy("resultsmap") %>%
        clearGroup("wells") %>%
        addCircleMarkers(~x, ~y, color = ~wellPal(selected), group = "wells",
                         data=mapclicks$well_locations)
      print("7")
      gridded <- anem::get_gridded_hydrodynamics(wells_utm,aquifer_utm,head_dim=c(100,100),flow_dim=c(5,5))
      cl <- get_contourlines(gridded$head %>% dplyr::rename(z=head_m),
                             type="sf",crs=proj4string_scenario)
      bounds_polygon <- use_anem_function("bounds_sf_to_polygon",bounds_sf=aquifer_utm$bounds)
      cl <- cl %>% sf::st_intersection(bounds_polygon)

      # head_range <- c(min(gridded$head_m),max(gridded$head_m))
      print(cl[1:10,])
      headPal <- colorNumeric(palette = "Blues",domain=cl$level)
      headPal_rev <- colorNumeric(palette = "Blues",domain=cl$level, reverse=TRUE)
      leafletProxy("resultsmap") %>%
        clearGroup("head_cl") %>%
        addPolylines(color=~headPal(level),opacity=1,weight=3, group="head_cl",
                     data=cl %>% sf::st_transform(crs=4326)) %>%
        addLegend("bottomright", pal = headPal_rev, values = ~level, group="head_cl",
                  title = "Head, m", data=cl,
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)),
                  opacity = 1 )
      if (input$include_gridded_head) {
        head_raster_utm <- rasterFromXYZ(gridded$head %>% dplyr::rename(z=head_m),crs=proj4string_scenario)
        head_raster_wgs <- head_raster_utm %>% projectRaster(crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
        leafletProxy("resultsmap") %>%
          addRasterImage(head_raster_wgs,opacity=0.2,layerId="gridded_head") %>%
          leafem::addImageQuery(head_raster_wgs, type="mousemove", layerId = "gridded_head")
      }
    }
    updateResults$next_view <- FALSE
    updateResults$update_now <- FALSE
  })

  output$wells <- renderPrint({print(mapclicks$well_locations)})
  output$bounds <- renderPrint({print(bounds$bounds_sf)})
  # output$clickbounds_rect <- renderPrint({print(bounds$edges_rectangular)})
  # output$clickwells <- renderPrint({print(mapclicks$well_locations)})
  output$aquifer <- renderPrint({print(aquifer())})
}

shinyApp(ui, server)
