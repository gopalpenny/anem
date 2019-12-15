# anem_ui.R
library(leaflet)
library(anem)
library(DT)

source("app-functions/anem_shiny_helpers.R")

wellPal <- colorFactor(palette = c("darkgreen","green"), domain = c(FALSE, TRUE))
boundPal <- colorFactor(palette = c("blue","black"), domain = c("NF", "CH"))

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
            selectInput("aquifer_input","Edit aquifer",
                        c("Aquifer properties"="properties",
                          "Aquifer boundaries"="boundaries")),

            tabsetPanel(
              type="pills",
              tabPanel(
                "Properties",fluid=TRUE,
                # conditionalPanel(
                #   condition = "input.usermode == 'aquifer' & input.aquifer_input == 'properties'",
                # h5("Aquifer type"),
                radioButtons("aquifer_type", NULL, #"Aquifer type",
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
  observeEvent(bound_types(),{
    # print(bounds$bounds_sf)
    if (!is.null(bounds$bounds_sf)) {
      bounds$bounds_sf <- bounds$bounds_sf %>% dplyr::mutate(bound_type=bound_types())
      # print(bounds$bounds_sf)
      leafletProxy("map") %>%
        clearGroup("bounds_rectangular") %>%
        addPolylines(color = ~boundPal(bound_type), group = "bounds_rectangular",
                     fillOpacity = 0, opacity = 1, weight = 3,
                     data=bounds$bounds_sf)
    }
  })
  output$boundtypes <- renderPrint({print(bound_types())})

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
  output$usermode_elements <- renderText({
    # paste("usermode:",input$usermode)
    switch(input$usermode,
           "aquifer" = "Aquifer boundaries (double click to edit)",
           "wells" = "Wells (double click to edit)")
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
      addScaleBar(position = c("bottomright"), options = scaleBarOptions()) %>%
      addLayersControl(baseGroups = c("Map","Satellite"),#overlayGroups = c("Red","Blue") ,
                       options = layersControlOptions(collapsed = FALSE)) %>%
      setView(lng = -86.252, lat = 41.676, zoom=8)
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
    mapclicks <- interpret_map_click(mapClick,clickOperation,mapclicks,well_input=well_input)
    if (input$usermode == "aquifer") {
      bounds$edges_user <- get_edges_from_vertices(mapclicks$bound_vertices)
      leafletProxy("map",data=mapclicks$bound_vertices) %>%
        clearGroup("boundvertices") %>% leaflet::clearGroup("bounds_rectangular") %>%
        addCircleMarkers(~x, ~y, color = "black", group = "boundvertices") %>%
        addPolygons(~x, ~y, color = "black", dashArray = "10 10", opacity = 0.3, weight = 2,
                    layerId = "boundedges",fillOpacity = 0)
      if (dim(bounds$edges_user)[1]==4) {
        bounds$edges_rectangular <-
          use_anem_function("get_utm_rectangle",
                            edges_user=bounds$edges_user) %>%
          dplyr::mutate(bound_type=bound_types()) %>%
          dplyr::select(bID,bound_type,dplyr::everything())
        bounds$bounds_sf <- use_anem_function("bounds_to_sf",bounds$edges_rectangular)
        # print(bounds$bounds_sf)
        leafletProxy("map") %>%
          clearGroup("boundvertices") %>%
          addPolylines(color = ~boundPal(bound_type), group = "bounds_rectangular",
                      fillOpacity = 0, opacity = 1, weight = 3,
                      data=bounds$bounds_sf)
      }
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
      # print("mapclicks$well_locations")
      # print(mapclicks$well_locations)
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
    mapclicks$bound_vertices <- data.frame(x=numeric(),y=numeric(),bID=integer())
    mapclicks$well_locations <- data.frame(Q=numeric(),R=numeric(),diam=numeric(),
                              x=numeric(),y=numeric(),wID=integer(),selected=logical())
    leafletProxy("map") %>%
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
      leafletProxy("map") %>%
        clearGroup("wells") %>%
        addCircleMarkers(~x, ~y, color = ~wellPal(selected), group = "wells",
                         data=mapclicks$well_locations)
    }
    # str(mapclicks$well_locations)
    # output$edited <- renderTable({mapclicks$well_locations})
  })


  # output$edgetable <- renderDataTable(
  #   datatable(bounds$edges_rectangular[,1:2],
  #             editable=T,rownames=F,
  #             options = list(searching=FALSE,
  #                            # formatNumber= function(x) format(x,nsmall=3),
  #                            lengthChange=FALSE,
  #                            autoWidth = TRUE,
  #                            columnDefs = list(list(width = '200px', targets = "_all")),
  #                            paging=FALSE,info=FALSE)
  #   ))

  # observeEvent(input$edgetable_cell_edit, {
  #   info <- input$edgetable_cell_edit
  #   str(info)
  #   i <- info$row
  #   j <- info$col + 1  # column index offset by 1
  #   v <- info$value
  #   bounds$edges_rectangular[i, j] <<- DT::coerceValue(v, bounds$edges_rectangular[i, j])
  #   replaceData(proxy_edgetable, bounds$edges_rectangular[,1:2], resetPaging = FALSE, rownames = F)
  #   bounds$bounds_sf <- use_anem_function("bounds_to_sf",bounds$edges_rectangular)
  #   # if (j %in% c(4,5)) { # update map if x or y change
  #   leafletProxy("map") %>%
  #     clearGroup("bounds_rectangular") %>%
  #     addPolylines(color = ~boundPal(bound_type), group = "bounds_rectangular",
  #                  fillOpacity = 0, opacity = 1, weight = 3,
  #                  data=bounds$bounds_sf)
  #   # }
  #   str(bounds$edges_rectangular)
  # })


  output$wells <- renderPrint({print(mapclicks$well_locations)})
  output$bounds <- renderPrint({print(bounds$bounds_sf)})
  # output$clickbounds_rect <- renderPrint({print(bounds$edges_rectangular)})
  # output$clickwells <- renderPrint({print(mapclicks$well_locations)})
  output$aquifer <- renderPrint({print(aquifer())})
}

shinyApp(ui, server)
