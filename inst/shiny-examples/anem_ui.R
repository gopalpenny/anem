# anem_ui.R
library(leaflet)
library(anem)
library(DT)

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
            numericInput("Ksat", "Saturated hydraulic conductivity, m/s^2",value = 0),
            numericInput("h0", "Undisturbed hydraulic head, m",value = 0),
            conditionalPanel(
              condition = "input.aquifer_type == 'confined'",
              numericInput("z0", "Aquifer thickness, m",0)
            )
          ),
          conditionalPanel(
            condition = "input.usermode == 'bounds' | input.usermode == 'wells'",
            # h5("Instructions"),
            textOutput("prepinstr"),
            column(4,
                   numericInput("Q","Q",0)
            ),
            column(4,
                   numericInput("R","R",0)
            ),
            column(4,
                   numericInput("diam","diam",0)
            )
          )
        ),
        # Prepare map
        column(8,
               h3(textOutput("maptitle")),
               leafletOutput("map",height=420)
        ),
      ),
      fluidRow(
        hr(),
        h4("Wells"),
        dataTableOutput("welltable"),
        hr(),
        verbatimTextOutput("clickbounds"),
        verbatimTextOutput("clickwells"),
        verbatimTextOutput("aquifer")
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
    text=NULL,
    clickedMarker=NULL,
    bound_vertices=data.frame(x=numeric(),y=numeric(),id=integer()),
    well_locations=data.frame(Q=numeric(),R=numeric(),diam=numeric(),
                              x=numeric(),y=numeric(),id=integer())
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
      # addProviderTiles("Esri.WorldImagery", group="background 1") %>%
      addTiles(options = providerTileOptions(noWrap = TRUE), group="background 2") %>%
      setView(lng = -120, lat = 37, zoom=7)
  })

  # store the click
  observeEvent(input$map_click,{
    clickedMarker <- input$map_click
    well_input <- list(Q=input$Q,R=input$R,diam=input$diam)
    mapclicks <- interpret_map_click(clickedMarker,input$usermode,mapclicks,well_input)
    if (input$usermode == "aquifer") {
      bound_edges$edges <- get_edges_from_vertices(mapclicks$bound_vertices)
      leafletProxy("map",data=mapclicks$bound_vertices) %>%
        clearGroup("boundvertices") %>%
        addCircleMarkers(~x, ~y, color = "black", group = "boundvertices") %>%
        addPolygons(~x, ~y, color = "black", layerId = "boundedges",fillOpacity = 0)
    } else if (input$usermode == "wells") {
      leafletProxy("map") %>%
        clearGroup("wells") %>%
        addCircleMarkers(~x, ~y, color = "blue", group = "wells",
                         data=mapclicks$well_locations)
    }
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
    str(mapclicks$well_locations)
    output$edited <- renderTable({mapclicks$well_locations})
  })

  # store the click
  observeEvent(input$map_marker_click,{
    clickedMarker <- input$map_marker_click
    output$current_well <- renderPrint({print(clickedMarker)})
  })

  output$clickbounds <- renderPrint({print(bound_edges$edges)})
  output$clickwells <- renderPrint({print(mapclicks$well_locations)})
  output$aquifer <- renderPrint({print(aquifer())})
}

shinyApp(ui, server)
