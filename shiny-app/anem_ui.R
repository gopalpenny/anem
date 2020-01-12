# anem_ui.R
library(leaflet)
library(anem)
library(DT)
library(ggplot2)
library(shinycssloaders)
library(raster)
library(mapview)
# library(tibble)

source("app-functions/anem_shiny_helpers.R")

wellPal <- colorFactor(palette = c("darkgreen","green"), domain = c(FALSE, TRUE))
# opacityFun <- function(x) switch(x+1,0.4,0.8)
boundPal <- colorFactor(palette = c("blue","black"), domain = c("NF", "CH"))

ui <- fluidPage(
  h4("anem ui"),
  # tags$style(HTML("
  #   .tabbable > .nav > li > a                  {background-color: white;  color:blue}
  #   .tabbable > .nav > li[class=active] > a                  {background-color: blue;  color:white}
  #   .tabbable > .nav > li > a[data-value='prepare'] {background-color: white;   color:#005A00}
  #   .tabbable > .nav > li[class=active] > a[data-value='prepare'] {background-color: #005A00; color:white}
  #   .tabbable > .nav > li > a[data-value='results'] {background-color: white;   color:#005A00}
  #   .tabbable > .nav > li[class=active] > a[data-value='results'] {background-color: #005A00; color:white}
  #   .tabbable > .nav > li > a[data-value='Properties'] {background-color: white;   color:#005A00}
  #   .tabbable > .nav > li[class=active] > a[data-value='Properties'] {background-color: #005A00; color:white}
  #   .tabbable > .nav > li > a[data-value='Boundaries'] {background-color: white;   color:#005A00}
  #   .tabbable > .nav > li[class=active] > a[data-value='Boundaries'] {background-color: #005A00; color:white}
  # ")),
  tabsetPanel(id="maintabs",
    type="tabs",
    tabPanel(
      "Instructions",value="instructions",fluid=TRUE,
      includeMarkdown("app-instructions/app-instructions.Rmd")
    ),
    tabPanel(
      "Prepare scenario",value="prepare",fluid=TRUE,
      fluidRow(
        # Prepare scenario
        # hr(),
        column(
          4,
          # hr(),
          HTML("<p style=font-size:45%><br></p>"),
          # verbatimTextOutput("utm_zone"),

          tabsetPanel(
            id="usermode",
            type="pills",
            tabPanel(
              "Define aquifer",value="aquifer",
              # radioButtons("usermode","Entry mode",
              #              c("Define aquifer" = "aquifer",
              #                # "Set aquifer bounds" = "bounds",
              #                "Add or modify wells" = "wells")),
              # conditionalPanel(

              hr(),
              tabsetPanel(
                type="tabs",
                tabPanel(
                  "Boundaries",fluid=TRUE,
                  HTML("<p style=font-size:45%><br></p>"),
                  #     "Prepare scenario",fluid=TRUE,
                  # conditionalPanel(
                  #   condition="input.usermode == 'aquifer' & input.aquifer_input == 'boundaries'",
                  # h4("Aquifer "),
                  p("Click 4 points to add rectangular aquifer boundaries."), #bound_type can be \"NF\" (no flow) or \"CH\" (constant head)"),
                  fluidRow(
                    # column(6,dataTableOutput("edgetable")),
                    column(5,h5("Bound 1:"),align='right'),
                    column(7,selectInput("b1_type",NULL,choices = c("No flow"="NF","Constant head"="CH"),selected = "No flow"))
                  ),
                  fluidRow(
                    # column(6,dataTableOutput("edgetable")),
                    column(5,h5("Bound 2:"),align='right'),
                    column(7,selectInput("b2_type",NULL,choices = c("No flow"="NF","Constant head"="CH"),selected = "No flow"))
                  ),
                  fluidRow(
                    # column(6,dataTableOutput("edgetable")),
                    column(5,h5("Bound 3:"),align='right'),
                    column(7,selectInput("b3_type",NULL,choices = c("No flow"="NF","Constant head"="CH"),selected = "No flow"))
                  ),
                  fluidRow(
                    # column(6,dataTableOutput("edgetable")),
                    column(5,h5("Bound 4:"),align='right'),
                    column(7,selectInput("b4_type",NULL,choices = c("No flow"="NF","Constant head"="CH"),selected = "No flow"))
                  )#,verbatimTextOutput("boundtypes")
                ),
                tabPanel(
                  "Aquifer properties",fluid=TRUE,
                  HTML("<p style=font-size:25%><br></p>"),
                  # conditionalPanel(
                  #   condition = "input.usermode == 'aquifer' & input.aquifer_input == 'properties'",
                  # h5("Aquifer type"),
                  radioButtons("aquifer_type", "Aquifer type", #"Aquifer type",
                               c("Confined" = "confined","Unconfined" = "unconfined")),
                  numericInput("Ksat", "Ksat, m/s^2",value = 0.001),
                  numericInput("h0", "Undisturbed head, m",value = 50),
                  conditionalPanel(
                    condition = "input.aquifer_type == 'confined'",
                    numericInput("z0", "Aquifer thickness, m",10)
                  )
                )
              )
            ),

            tabPanel(
              "Edit wells",value="wells",
              hr(),
              # h5("Instructions"),
              textOutput("prepinstr"),
              HTML("<p style=font-size:45%><br></p>"),
              tabsetPanel(
                id="welltab",
                type="tabs",
                tabPanel(
                  "New well",value="newwell",
                  HTML("<p style=font-size:45%><br></p>"),
                  HTML("<p>Set Q (-) for abstraction, (+) for injection.</p>"),
                  fluidRow(
                    column(6,numericInput("Q","Q (cumec)",-0.1)),
                    # column(4,numericInput("R","R (m)",9000)),
                    column(6,numericInput("diam","diam (m)",1))
                  ),
                  fluidRow(
                    column(6,textInput("well_group", "Group",value = "A")),
                    column(6,numericInput("well_weight","Weight",value = 1))
                  ),
                  hr(),
                  fluidRow(
                    column(6,actionButton("deleteWell","Delete selected well"),offset=3)
                  )
                ),
                tabPanel(
                  "Radius of Influence",value="wellROI",
                  HTML("<p style=font-size:45%><br></p>"),
                  HTML(paste("<p>A well contributes to drawdown only within its radius of influence.",
                             "See <a href=http://www.doi.org/10.7343/AS-117-15-0144>Fileccia, 2015</a>.</p>")),
                  conditionalPanel(
                    condition="input.aquifer_type == 'confined'",
                    HTML("<p>For <b>confined</b> aquifers (current selection), this can be approximated as</p>"),
                    uiOutput("roi_confined"),
                    HTML(paste0("<p><font face='consolas'>", # courier
                                "t: Elapsed time of pumping<br>",
                                "S: Aquifer storativity</font></p>"))
                  ),
                  conditionalPanel(
                    condition="input.aquifer_type == 'unconfined'",
                    HTML("<p>For <b>unconfined</b> aquifers (current selection), this can be approximated as</p>"),
                    uiOutput("roi_unconfined"),
                    HTML(paste0("<p><font face='consolas'>", # courier
                                "t: Elapsed time of pumping<br>",
                                "n: Aquifer porosity</font></p>"))
                  ),
                  fluidRow(
                    column(6,numericInput("pumpingtime","t, months",12,0,12*100)),
                    conditionalPanel(
                      condition="input.aquifer_type == 'confined'",
                      column(6,numericInput("storativity","S, unitless",0.35,0,1,0.01))
                    ),
                    conditionalPanel(
                      condition="input.aquifer_type == 'unconfined'",
                      column(6,numericInput("porosity","Aquifer porosity, n",0.35,0,1,0.01))
                    )
                  )
                )
              )
            )
          )
        ),
        # Prepare map
        column(8,
               fluidRow(
                 column(4,h3(textOutput("prepmaptitle"))),
                 # column(4,actionButton("resetMap","Reset map",style='padding:4px; font-size:80%'),offset=4),
                 column(4,align='right',
                        HTML("<p style=font-size:45%><br><br></p>"),
                        actionLink("resetMap","Reset map",style='font-size:80%'),offset=4)
               ),
               leafletOutput("prepmap",height=420)
        ),
      ),
      hr(),
      # h4(textOutput("usermode_elements")),
      h4("Wells (double click to edit)"),
      fluidRow(
        column(12,
               conditionalPanel(
                 condition="input.usermode == 'wells'",
                 dataTableOutput("welltable"))
        )
      ),
      hr(),
      verbatimTextOutput("aquifer"),
      verbatimTextOutput("bounds"),
      verbatimTextOutput("wells")

    ),
    tabPanel(
      "View results",value="results",fluid=TRUE,
      fluidRow(
        # hr(),
        column(4,
               checkboxInput("linkmaps","Link maps",TRUE)#,
               # checkboxInput("include_gridded_head","Gridded head, m",FALSE),
               # conditionalPanel(condition= 'input.include_gridded_head',
               #                  sliderInput("head_opacity","Opacity",min=0,max=100,value=100)
               # )
        ),
        column(8,
               fluidRow(
                 column(4,h3("Results"))#,
                 # column(4,actionButton("donothing","Does nothing",style='padding:4px; font-size:80%'),offset=4)
               ),
               leafletOutput("resultsmap",height=420) %>% withSpinner()#,
               # plotOutput("results")
        )
      ),
      hr(),
      # h4(textOutput("usermode_elements")),
      h4("Wells (double click to edit)"),
      fluidRow(
        column(4,
               dataTableOutput("welltable2")
        ),
        column(4,
               dataTableOutput("welltable_head")
        )
      ),
      tableOutput("drawdown")
    )
  )

)

server <- function(input, output) {

  # Check when to update results
  updateResults <- reactiveValues(
    next_view=TRUE,
    generate_well_images_now=TRUE,
    update_results_now=FALSE
  )

  # Initialize reactive values
  mapclicks <- reactiveValues(
    bound_vertices=data.frame(bID=integer(),#bound_type=factor(levels=c("NF","CH")),
                              x=numeric(),y=numeric()),
    well_locations=data.frame(Q=numeric(),R=numeric(),diam=numeric(),Group=character(),Weight=numeric(),
                              x=numeric(),y=numeric(),wID=integer(),selected=logical(),stringsAsFactors = FALSE)
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

  wells <- reactiveValues(
    utm = NULL,
    head=data.frame(head_m=0)
  )

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

  proj4string_scenario <- reactive({
    if (is.null(utm_zone())) {
      p4s <- NULL
    } else {
      p4s <- anem::utm_zone_to_proj4(utm_zone())
    }
    p4s
  })

  gridded <- reactiveValues(
    h=NULL,
    raster_utm = NULL,
    raster_wgs = NULL
  )

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
  # output$usermode_elements <- renderText({
  #   # paste("usermode:",input$usermode)
  #   switch(input$usermode,
  #          "aquifer" = "Aquifer boundaries (double click to edit)",
  #          "wells" = "Wells (double click to edit)")
  # })

  newwellROI <- reactive({
    current_aquifer <- aquifer()
    Ksat <- current_aquifer$Ksat
    h0 <- current_aquifer$h0
    switch(input$aquifer_type,
           "confined"=round(sqrt(Ksat*h0*input$pumpingtime*30*24*3600/input$storativity)),
           "unconfined"=round(sqrt(Ksat*h0*input$pumpingtime*30*24*3600/input$porosity)))
  })

  output$roi_confined <- renderUI({
    withMathJax(paste0("$$","R = \\sqrt{2.25 K_{sat} h_0 t / S}=",
                       round(newwellROI()),"\\text{ m}$$")) # convert months to seconds as 30*24*3600
  })
  output$roi_unconfined <- renderUI({
    withMathJax(paste0("$$","R = \\sqrt{1.9 K_{sat} h_0 t / n}=",
                       round(newwellROI()),"\\text{ m}$$")) # convert months to seconds as 30*24*3600
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
      # leafem::addMouseCoordinates() %>%
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
    well_input <- list(Q=input$Q,R=newwellROI(),diam=input$diam,group=input$well_group,weight=input$well_weight)
    mapclicks <- interpret_map_click(prepmapClick,clickOperation,mapclicks,well_input=well_input)
    # print(mapclicks$well_locations %>% tibble::as_tibble())
    if (input$usermode == "aquifer") {
      bounds$edges_user <- get_edges_from_vertices(mapclicks$bound_vertices)
      leafletProxy("prepmap",data=mapclicks$bound_vertices) %>%
        clearGroup("boundvertices") %>% leaflet::clearGroup("bounds_rectangular") %>%
        addCircleMarkers(~x, ~y, color = "black", group = "boundvertices", opacity = 0.5) %>%
        addPolygons(~x, ~y, color = "black", dashArray = "10 10", opacity = 0.3, weight = 2,
                    layerId = "boundedges",fillOpacity = 0)
      if (dim(bounds$edges_user)[1]==4) {
        # print("1")
        bounds$edges_rectangular <-
          use_anem_function("get_utm_rectangle",
                            edges_user=bounds$edges_user) %>%
          dplyr::mutate(bound_type=bound_types()) %>%
          dplyr::select(bID,bound_type,dplyr::everything()) %>%
          dplyr::arrange(bID)
        # print("2")
        bounds$bounds_sf <- use_anem_function("bounds_to_sf",bounds$edges_rectangular,crs=4326)
        # print("3")
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
        addCircleMarkers(~x, ~y, color = ~wellPal(selected), group = "wells", opacity = 1, radius = 5,
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
        addCircleMarkers(~x, ~y, color = ~wellPal(selected), group = "wells", opacity = 1, radius = 5,
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
      addCircleMarkers(~x, ~y, color = ~wellPal(selected), group = "wells", opacity = 1, radius = 5,
                       data=mapclicks$well_locations)
  })

  observeEvent(input$resetMap,{
    mapclicks$bound_vertices <- data.frame(x=numeric(),y=numeric(),bID=integer())
    mapclicks$well_locations <- data.frame(Q=numeric(),R=numeric(),diam=numeric(),group=character(),weight=numeric(),
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

  output$welltable2 <- renderDataTable(
    datatable(mapclicks$well_locations %>% dplyr::select(Q,diam,Group,selected),
              editable=T,rownames=F,
              options = list(searching=FALSE,
                             # formatNumber= function(x) format(x,nsmall=3),
                             lengthChange=FALSE,
                             autoWidth = TRUE,
                             columnDefs = list(list(width = '200px', targets = "_all")))
    ) %>%
      formatStyle('selected',target='row',
                  backgroundColor = styleEqual(c(FALSE,TRUE),c('white','lightgreen'))))

  output$welltable_head <- renderDataTable(
    datatable(wells$head,
              rownames=F,
              options = list(searching=FALSE,
                             # formatNumber= function(x) format(x,nsmall=3),
                             lengthChange=FALSE,
                             autoWidth = TRUE,
                             columnDefs = list(list(width = '200px', targets = "_all")))
    )
  )


  # output$wellDT <- renderDT(mapclicks$well_locations, selection = 'none', rownames = T, editable = T)
  #
  proxy_welltable <- dataTableProxy('welltable')
  proxy_welltable2 <- dataTableProxy('welltable2')
  proxy_welltable_head <- dataTableProxy('welltable_head')
  # proxy_edgetable <- dataTableProxy('edgetable')

  observeEvent(input$welltable_cell_edit, {
    info = input$welltable_cell_edit
    # str(info)
    i = info$row
    j = info$col + 1  # column index offset by 1
    v = info$value
    mapclicks$well_locations[i, j] <<- DT::coerceValue(v, mapclicks$well_locations[i, j])
    replaceData(proxy_welltable, mapclicks$well_locations, resetPaging = FALSE, rownames = F)
    replaceData(proxy_welltable2, mapclicks$well_locations %>% dplyr::select(Q,diam,Group,selected), resetPaging = FALSE, rownames = F)
    if (j %in% c(6,7)) { # update map if x or y change
      leafletProxy("prepmap") %>%
        clearGroup("wells") %>%
        addCircleMarkers(~x, ~y, color = ~wellPal(selected), group = "wells",
                         data=mapclicks$well_locations)
    }
  })

  observeEvent(input$welltable2_cell_edit, {
    info = input$welltable2_cell_edit
    # str(info)
    i = info$row
    # print("info$col")
    # print(info$col)
    j_click = info$col + 1  # column index offset by 1
    v = info$value
    j <- switch(j_click,1,3,4)
    mapclicks$well_locations[i, j] <<- DT::coerceValue(v, mapclicks$well_locations[i, j])
    replaceData(proxy_welltable, mapclicks$well_locations, resetPaging = FALSE, rownames = F)
    replaceData(proxy_welltable2, mapclicks$well_locations %>% dplyr::select(Q,diam,Group,selected), resetPaging = FALSE, rownames = F)

    # Update all image wells
    wID <- mapclicks$well_locations$wID[i]
    # print("wells$utm_with_images")
    # print(wells$utm_with_images)
    wells$utm_with_images$Q[wells$utm_with_images$wID==wID] <- mapclicks$well_locations[i, j]
    wells$utm_with_images <- anem::reconstruct_image_pumping(wells$utm_with_images)
    # print(wells$utm_with_images)

    # update results
    updateResults$update_results_now <- TRUE
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
      updateResults$generate_well_images_now <- TRUE
    }
  })

  observeEvent(updateResults$generate_well_images_now,{
    print("observeEvent(updateResults$generate_well_images_now)")
    print('nrow mapclicks$well_locations')
    print(nrow(mapclicks$well_locations))
    if (nrow(mapclicks$well_locations) > 0) {
      wells_wgs <- mapclicks$well_locations %>%
        sf::st_as_sf(coords=c("x","y"),crs=4326)
    } else {
      wells_wgs <- NULL
    }
    # print("2")
    bounds_utm <- NULL
    # if there are bounds, map them and get proj4string
    if(!is.null(bounds$bounds_sf)) {
      bounds_utm <- bounds$bounds_sf %>%
        dplyr::select(-dplyr::matches('^[mb]$'),-dplyr::matches("[xy][12]")) %>%
        sf::st_transform(anem::utm_zone_to_proj4(utm_zone()))
      # print("2.1")
      aquifer_utm <- aquifer()
      # print("2.2")
      # saveRDS(bounds_utm,"app-debug/bounds_utm.rds")
      # print("2.3")
      aquifer_utm$bounds <- define_bounds(bounds_utm)
      # print("3")
      # print(bounds$bounds_sf)
      leafletProxy("resultsmap") %>%
        clearGroup("bounds_rectangular") %>%
        clearControls() %>%
        addPolylines(color = ~boundPal(bound_type), group = "bounds_rectangular",
                     fillOpacity = 0, opacity = 1, weight = 4,
                     data=bounds$bounds_sf)
      # print('4')
      # if no bounds, get proj4string from wells
    } else {
      # print('3x')
      if(!is.null(wells_wgs)) {
        aquifer_utm <- aquifer()
      }
    }
    if (!is.null(wells_wgs)) {
      wells_utm <- wells_wgs %>%
        sf::st_transform(crs=proj4string_scenario())

      # filter for only wells inside the aquifer
      if (!is.null(aquifer_utm$bounds)) {
        bounds_polygon <- use_anem_function("bounds_sf_to_polygon",bounds_sf=aquifer_utm$bounds)
        wells_utm_orig <- wells_utm
        wells_utm <- sf::st_intersection(wells_utm,bounds_polygon)
        if (!identical(wells_utm,wells_utm_orig)) {
          warning(dim(wells_utm_orig)[1]-dim(wells_utm)[1]," wells were outside the aquifer boundary. They have been removed.")
          mapclicks$well_locations <- mapclicks$well_locations %>% dplyr::filter(wID %in% wells_utm$wID)
        }
      }

      # generate well images
      wells$utm_with_images <- wells_utm %>%
        define_wells() %>%
        generate_image_wells(aquifer_utm)
      # print("n wells")
      # print(wells$utm_with_images)
      leafletProxy("resultsmap") %>%
        clearGroup("wells") %>%
        addCircleMarkers(~x, ~y, color = ~wellPal(selected), group = "wells",
                         data=mapclicks$well_locations)
      # print("7")
    }
    updateResults$update_results_now <- TRUE
    updateResults$next_view <- FALSE
    updateResults$generate_well_images_now <- FALSE
  })

  observeEvent(updateResults$update_results_now,{
    print("observeEvent(updateResults$update_results_now)")
    aquifer_utm <- aquifer()
    bounds_utm <- NULL
    if(!is.null(bounds$bounds_sf)) {
      bounds_utm <- bounds$bounds_sf %>%
        dplyr::select(-dplyr::matches('^[mb]$'),-dplyr::matches("[xy][12]")) %>%
        sf::st_transform(anem::utm_zone_to_proj4(utm_zone()))
      aquifer_utm$bounds <- define_bounds(bounds_utm)
    }
    if (!is.null(wells$utm_with_images)) {
      # print('1')
      # print('aquifer_utm')
      # print(aquifer_utm)

      # get head at wells
      head_wells <- wells$utm_with_images %>% dplyr::filter(wID==orig_wID) %>%
        anem::get_hydraulic_head(wells$utm_with_images,aquifer_utm)
      # print(head_wells)
      wells$head <- wells$utm_with_images %>% dplyr::filter(wID==orig_wID) %>%
        dplyr::select(wID) %>% sf::st_set_geometry(NULL) %>% dplyr::mutate(head_m=round(head_wells,3))
      # print("wells$head")
      # print(wells$head)
      replaceData(proxy_welltable_head, wells$head, resetPaging = FALSE, rownames = F)

      # get gridded head
      gridded$h <- anem::get_gridded_hydrodynamics(wells$utm_with_images,aquifer_utm,head_dim=c(100,100),flow_dim=c(5,5))
      gridded$raster_utm <- rasterFromXYZ(gridded$h$head %>% dplyr::rename(z=head_m),crs=proj4string_scenario())
      gridded$raster_wgs <-  gridded$raster_utm %>% projectRaster(crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

      gridded$head_wgs_shp <- raster::rasterToPolygons(gridded$raster_wgs,dissolve = TRUE)

      # get contour lines of head
      cl <- get_contourlines(gridded$h$head %>% dplyr::rename(z=head_m),
                             type="sf",crs=proj4string_scenario())
      bounds_polygon <- use_anem_function("bounds_sf_to_polygon",bounds_sf=aquifer_utm$bounds)
      cl <- cl %>% sf::st_intersection(bounds_polygon)

      # head_range <- c(min(gridded$h$head_m),max(gridded$h$head_m))
      headPal <- colorNumeric(palette = "Blues",domain=cl$level, reverse=TRUE)
      headPal_rev <- colorNumeric(palette = "Blues",domain=cl$level)
      leafletProxy("resultsmap") %>%
        clearGroup("head_cl") %>%
        clearControls() %>%
        addPolylines(color=~headPal(level),opacity=1,weight=3, group="head_cl", label=~as.character(head_label),
                     data=cl %>% sf::st_transform(crs=4326) %>% dplyr::mutate(head_label=paste0("Head: ",round(level,2)," m"))) %>%
        addLegend("bottomright", pal = headPal_rev, values = ~level, group="head_cl",
                  title = "Head, m", data=cl,
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)),
                  opacity = 1 )

    }
    updateResults$update_results_now <- FALSE
  })

  # observe({
  #   if (input$include_gridded_head) {
  #     leafletProxy("resultsmap") %>%
  #       clearGroup("gridded_head") %>%
  #       addRasterImage(gridded$raster_utm,layerId="griddedhead",group = "gridded_head",opacity=input$head_opacity/100) # %>%
  #     # leafem::addImageQuery(head_raster_utm, type="mousemove", project=TRUE,
  #     #                       layerId = "griddedhead",position="bottomleft")
  #   } else {
  #     leafletProxy("resultsmap") %>%
  #       clearGroup("gridded_head")
  #   }
  # })

  output$wells <- renderPrint({print(mapclicks$well_locations) %>% tibble::as_tibble()})
  output$bounds <- renderPrint({print(bounds$bounds_sf)})
  # output$clickbounds_rect <- renderPrint({print(bounds$edges_rectangular)})
  # output$clickwells <- renderPrint({print(mapclicks$well_locations)})
  output$aquifer <- renderPrint({print(aquifer())})

  output$drawdown <- renderTable({
    get_drawdown_relationships(wells$utm_with_images,aquifer(),group_column = Group, weights_column = Weight)
  })
}

shinyApp(ui, server)
