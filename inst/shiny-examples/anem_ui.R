# anem_ui.R
library(leaflet)
library(anem)
library(DT)

wellPal <- colorFactor(palette = c("blue","darkgreen"), domain = c(FALSE, TRUE))

ui <- fluidPage(
  h4("anem ui"),
  tabsetPanel(
    type="pills",
    tabPanel(
      "Prepare scenario",fluid=TRUE,
      fluidRow(
        # Prepare scenario
        hr(),
        column(
          4,
          radioButtons("usermode","Entry mode",
                       c("Define aquifer" = "aquifer",
                         # "Set aquifer bounds" = "bounds",
                         "Add or modify wells" = "wells")),
          conditionalPanel(
            condition = "input.usermode == 'aquifer'",
            # h5("Aquifer type"),
            radioButtons("aquifer_type", "Aquifer type",
                         c("Confined" = "confined","Unconfined" = "unconfined")),
            numericInput("Ksat", "Saturated hydraulic conductivity, m/s^2",value = 0.001),
            numericInput("h0", "Undisturbed hydraulic head, m",value = 50),
            conditionalPanel(
              condition = "input.aquifer_type == 'confined'",
              numericInput("z0", "Aquifer thickness, m",10)
            )
          ),
          conditionalPanel(
            condition = "input.usermode == 'bounds' | input.usermode == 'wells'",
            # h5("Instructions"),
            textOutput("prepinstr"),
            h4("New well:"),
            fluidRow(
              column(4,numericInput("Q","Q",0.1)),
              column(4,numericInput("R","R",1000)),
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
                 column(4,h3(textOutput("maptitle"))),
                 column(4,actionButton("resetMap","Reset map",style='padding:4px; font-size:80%'),offset=4)
               ),
               leafletOutput("map",height=420)
        ),
      ),
      fluidRow(
        hr(),
        h4("Wells (double click to edit)"),
        dataTableOutput("welltable"),
      #   hr(),
      #   verbatimTextOutput("clickbounds"),
      #   verbatimTextOutput("clickwells"),
      #   verbatimTextOutput("aquifer")
      )

    ),
    tabPanel("Output",fluid=TRUE,
             column(6,
             ),
             column(6,
                    #LEAFLETMAP
             )

    )
  )

)

server <- function(input, output) {
  mapclicks <- reactiveValues(
    bound_vertices=data.frame(x=numeric(),y=numeric(),id=integer()),
    well_locations=data.frame(Q=numeric(),R=numeric(),diam=numeric(),
                              x=numeric(),y=numeric(),id=integer(),selected=logical())
  )

  bound_edges <- reactiveValues(
    edges = NULL
  )

  aquifer <- reactive({define_aquifer(
    aquifer_type=input$aquifer_type,
    h0=input$h0,
    Ksat=input$Ksat,
    z0=input$z0)})

  output$prepinstr <- renderText({
    # paste("usermode:",input$usermode)
    switch(input$usermode,
           "aquifer" = "Set aquifer properties.",
           "bounds" = "Click 4 vertices on the map to define the aquifer.",
           "wells" = "Click a well to edit, or click an empty space to add a well.")
  })
  output$maptitle <- renderText({
    # paste("usermode:",input$usermode)
    switch(input$usermode,
           "aquifer" = "Define aquifer",
           "wells" = "Define wells")
  })

  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(options = providerTileOptions(noWrap = TRUE), group="Map") %>%
      addProviderTiles("Esri.WorldImagery", group="Satellite") %>%
      addLayersControl(baseGroups = c("Map","Satellite"),#overlayGroups = c("Red","Blue") ,
                       options = layersControlOptions(collapsed = FALSE)) %>%
      setView(lng = -120, lat = 37, zoom=7)
  })

  # Map click (new well)
  observeEvent(input$map_click,{
    mapClick <- input$map_click
    if (!is.null(input$map_marker_click)) {
      markerClick <- input$map_marker_click
    } else {
      markerClick <- list(lat=Inf,lng=Inf)
    }
    map_marker_equal <- identical(mapClick[c("lng","lat")],markerClick[c("lng","lat")])
    clickType <- dplyr::case_when(
      map_marker_equal ~ "marker",
      !map_marker_equal ~ "map")
    clickOperation <- dplyr::case_when(
      input$usermode=="aquifer" ~ "aquifer_vertex",
      input$usermode=="wells" & clickType=="map" ~ "new_well",
      input$usermode=="wells" & clickType=="marker" ~ "edit_well"
    )
    well_input <- list(Q=input$Q,R=input$R,diam=input$diam)
    mapclicks <- interpret_map_click(mapClick,clickOperation,mapclicks,well_input)
    if (input$usermode == "aquifer") {
      bound_edges$edges <- get_edges_from_vertices(mapclicks$bound_vertices)
      leafletProxy("map",data=mapclicks$bound_vertices) %>%
        clearGroup("boundvertices") %>%
        addCircleMarkers(~x, ~y, color = "black", group = "boundvertices") %>%
        addPolygons(~x, ~y, color = "black", layerId = "boundedges",fillOpacity = 0)
    } else if (clickOperation == "new_well") {
      leafletProxy("map") %>%
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
      print("mapclicks$well_locations")
      print(mapclicks$well_locations)
      leafletProxy("map") %>%
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
    leafletProxy("map") %>%
      clearGroup("wells") %>%
      addCircleMarkers(~x, ~y, color = ~wellPal(selected), group = "wells",
                       data=mapclicks$well_locations)
  })

  observeEvent(input$resetMap,{
    mapclicks$bound_vertices <- data.frame(x=numeric(),y=numeric(),id=integer())
    mapclicks$well_locations <- data.frame(Q=numeric(),R=numeric(),diam=numeric(),
                              x=numeric(),y=numeric(),id=integer(),selected=logical())
    leafletProxy("map") %>%
      clearShapes() %>% clearMarkers()
  })

  output$welltable <- renderDataTable(
    {mapclicks$well_locations},
    options = list(searching=FALSE,
                   # formatNumber= function(x) format(x,nsmall=3),
                   lengthChange=FALSE,
                   autoWidth = TRUE,
                   columnDefs = list(list(width = '200px', targets = "_all"))),
    editable=T,rownames=F)


  # output$wellDT <- renderDT(mapclicks$well_locations, selection = 'none', rownames = T, editable = T)
  #
  proxy = dataTableProxy('welltable')

  observeEvent(input$welltable_cell_edit, {
    info = input$welltable_cell_edit
    str(info)
    i = info$row
    j = info$col + 1  # column index offset by 1
    v = info$value
    mapclicks$well_locations[i, j] <<- DT::coerceValue(v, mapclicks$well_locations[i, j])
    replaceData(proxy, mapclicks$well_locations, resetPaging = FALSE, rownames = F)
    if (j %in% c(4,5)) {
      leafletProxy("map") %>%
        clearGroup("wells") %>%
        addCircleMarkers(~x, ~y, color = ~wellPal(selected), group = "wells",
                         data=mapclicks$well_locations)
    }
    # str(mapclicks$well_locations)
    # output$edited <- renderTable({mapclicks$well_locations})
  })


  output$clickbounds <- renderPrint({print(bound_edges$edges)})
  output$clickwells <- renderPrint({print(mapclicks$well_locations)})
  output$aquifer <- renderPrint({print(aquifer())})
}

shinyApp(ui, server)
