# anem_ui.R
library(leaflet)
library(anem)
library(DT)
library(ggplot2)
library(shinycssloaders)
library(raster)
library(mapview)
library(deSolve)
# library(tibble)

source("app-functions/anem_shiny_helpers.R")

wellPal <- colorFactor(palette = c("darkgreen","green"), domain = c(FALSE, TRUE))
partPal <- colorFactor(palette = c("darkred","red"), domain = c(FALSE, TRUE))
# opacityFun <- function(x) switch(x+1,0.4,0.8)
boundPal <- colorFactor(palette = c("blue","black"), domain = c("NF", "CH"))

ui <- fluidPage(
  h4("anem-app (Beta version)"),
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
              "Aquifer",value="aquifer",
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
                  fluidRow(
                    column(6,selectInput("aquifer_type", "Aquifer type", #"Aquifer type",
                                          c("Confined" = "confined","Unconfined" = "unconfined"))),
                    column(6,numericInput("porosity","Aquifer porosity, n",0.35,0,1,0.01))
                  ),
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
              "Wells",value="wells",
              hr(),
              # h5("Instructions"),
              p("Click a well to edit, or click an empty space to add a well."),
              HTML("<p style=font-size:45%><br></p>"),
              tabsetPanel(
                id="welltab",
                type="tabs",
                tabPanel(
                  "Edit wells",value="newwell",
                  HTML("<p style=font-size:45%><br></p>"),
                  HTML("<p><b>New wells:</b> Set Q (-) for abstraction, (+) for injection.</p>"),
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
                             "See <a href=http://www.doi.org/10.7343/AS-117-15-0144 target=\"_blank\">Fileccia, 2015</a>.</p>")),
                  conditionalPanel(
                    condition="input.aquifer_type == 'confined'",
                    HTML(paste0("For <b>confined</b> aquifers (current selection), this can be approximated as</p>")),
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
                    column(6,numericInput("pumpingtime","t, months",64,0,12*100)),
                    conditionalPanel(
                      condition="input.aquifer_type == 'confined'",
                      column(6,numericInput("storativity","S, unitless",0.35,0,1,0.01))
                    ),
                    conditionalPanel(
                      condition="input.aquifer_type == 'unconfined'",
                      column(6,numericInput("porosity_roi","Aquifer porosity, n",0.35,0,1,0.01))
                    )
                  )
                )
              )
            ),
            tabPanel(
              "Particles",value="particles",
              hr(),
              p("Click map to set initial locations for particle tracking."),
              hr(),
              fluidRow(
                column(6,numericInput("max_tracking_time_years","Max time, years",value=10,min=0)),
                column(6,
                       HTML("<p style=font-size:45%><br></p>"),
                       actionButton("deleteParticle","Delete particle"))
              ),
              dataTableOutput("particletable"),
              HTML("<p style=font-size:100%><br></p>"),
            )
          )
        ),
        # Prepare map
        column(8,
               fluidRow(
                 column(8,h3(textOutput("prepmaptitle"))),
                 # column(4,actionButton("resetMap","Reset map",style='padding:4px; font-size:80%'),offset=4),
                 column(4,align='right',
                        HTML("<p style=font-size:45%><br><br></p>"),
                        actionLink("resetMap","Reset map",style='font-size:80%'))
               ),
               leafletOutput("prepmap",height=430),
               fluidRow(
                 column(3,checkboxInput("update_images","Well images",FALSE)),
                 column(3,checkboxInput("update_head","Hydraulic head",FALSE)),
                 column(3,checkboxInput("update_particles","Particle tracking",FALSE)),
                 column(3,checkboxInput("linkmaps","Link maps",TRUE))
               )
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
               # checkboxInput("include_gridded_head","Gridded head, m",FALSE),
               # conditionalPanel(condition= 'input.include_gridded_head',
               #                  sliderInput("head_opacity","Opacity",min=0,max=100,value=100)
               # )
               hr(),
               h4("Particle tracking"),
               dataTableOutput("particletable_output")
        ),
        column(8,
               fluidRow(
                 column(12,h3(textOutput("resultsmaptitle")))#,
                 # column(4,actionButton("donothing","Does nothing",style='padding:4px; font-size:80%'),offset=4)
               ),
               leafletOutput("resultsmap",height=430) %>% withSpinner(),
               fluidRow(
                 column(3,checkboxInput("update_images_results","Well images",FALSE)),
                 column(3,checkboxInput("update_head_results","Hydraulic head",FALSE)),
                 column(3,checkboxInput("update_particles_results","Particle tracking",FALSE)),
                 column(3,checkboxInput("linkmaps_results","Link maps",TRUE))
               )
        )
      ),
      hr(),
      # h4(textOutput("usermode_elements")),
      h4("Wells (double click to edit)"),
      fluidRow(
        column(6,
               dataTableOutput("welltable2")
        ),
        column(6,
               dataTableOutput("welltable_head")
        )
      ),
      tableOutput("drawdowntable")
    )
  )

)

server <- function(input, output, session) {
  notification_id <- NULL

  # Check when to update results
  updateResults <- reactiveValues(
    wells_next_view=TRUE,
    particles_next_view=TRUE,
    update_images_now = 0,
    update_head_now = 0,
    # update_particle_tracking
    update_particles_now = 0
  )

  # Initialize reactive values
  mapclicks <- reactiveValues(
    bound_vertices=data.frame(bID=integer(),#bound_type=factor(levels=c("NF","CH")),
                              x=numeric(),y=numeric()),
    well_locations=data.frame(Q=numeric(),R=numeric(),diam=numeric(),Group=character(),Weight=numeric(),
                              x=numeric(),y=numeric(),wID=integer(),selected=logical(),stringsAsFactors = FALSE),
    particle_locations=data.frame(pID=integer(),x=numeric(),y=numeric(),
                                  selected=logical(),stringsAsFactors = FALSE)
    # well_ROIs=
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
    n=input$porosity,
    bounds=bounds$bounds_sf)})

  wells <- reactiveValues(
    utm_with_images = NULL,
    head=data.frame(wID=NA,
                    `Head, m`=Inf,
                    `Drawdown, m`=0),
    drawdown_relationships=data.frame(var=character(),pot=numeric(),description=character())
  )

  wells_utm <- reactive({
    if (nrow(mapclicks$well_locations) > 0 & !is.null(proj4string_scenario())) {
      x <- mapclicks$well_locations %>%
        sf::st_as_sf(coords=c("x","y"),crs=4326) %>%
        sf::st_transform(crs=proj4string_scenario())
    } else {
      x <- NULL
    }
    x
  })

  particles <- reactiveValues(
    paths_wgs = NULL,
    tracking = data.frame(pID=integer(),time_years=numeric(),status=character(),x_end=numeric(),y_end=numeric())
  )

  particles_utm <- reactive({
    if (nrow(mapclicks$particle_locations) > 0 & !is.null(proj4string_scenario())) {
      x <- mapclicks$particle_locations %>%
        sf::st_as_sf(coords=c("x","y"),crs=4326) %>%
        sf::st_transform(crs=proj4string_scenario())
      coords_tbl <- sf::st_coordinates(x) %>% tibble::as_tibble() %>% dplyr::rename(x=X,y=Y)
      x <- x %>% dplyr::bind_cols(coords_tbl) %>%
        dplyr::select(pID,x,y,dplyr::everything()) ## FIX -- select only columns that appear in prep scenario tab
    } else {
      x <- NULL
    }
    x
  })

  wells_roi <- reactive({
    if (!is.null(wells_utm()) & !is.null(proj4string_scenario())) {
      coords <- tibble::as_tibble(wells_utm() %>% sf::st_coordinates())
      roi <- wells_utm() %>%
        dplyr::bind_cols(coords) %>%
        dplyr::rename(x=X,y=Y,r=R) %>%
        gen_circles() %>%
        sf::st_as_sf(coords=c("x","y"),crs=proj4string_scenario()) %>%
        dplyr::group_by(id) %>% dplyr::summarize(do_union=FALSE) %>%
        sf::st_cast("MULTILINESTRING") %>%
        sf::st_cast("MULTIPOLYGON")
    } else {
      roi <- NULL
    }
    roi
  })

  utm_zone <- reactive({
    if(is.null(bounds$bounds_sf)) {
      zone <- NULL
    } else if (nrow(bounds$bounds_sf) == 0) {
      zone <- NULL
    } else {
      centroid <- bounds$bounds_sf %>% sf::st_coordinates() %>% colMeans()
      zone <- anem::longitude_to_utm_zone(centroid[1])
    }
    if (is.null(zone)) {
      if (nrow(mapclicks$well_locations) > 0) { # get zone from wells
        centroid <- mapclicks$well_locations %>% dplyr::pull(x) %>% mean()
        zone <- anem::longitude_to_utm_zone(centroid)
      } else if (nrow(mapclicks$particle_locations) > 0) { # get zone from particles
        centroid <- mapclicks$particle_locations %>% dplyr::pull(x) %>% mean()
        zone <- anem::longitude_to_utm_zone(centroid)
      }
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
    if (!is.null(wells_utm())) {
      updateCheckboxInput(session,"update_images",value=TRUE)
      updateCheckboxInput(session,"update_head",value=TRUE)
    }
    if (!is.null(particles_utm())) {
      updateCheckboxInput(session,"update_particles",value=TRUE)
    }
  })
  observeEvent(mapclicks$well_locations,{
    if (input$maintabs=="prepare") {
      updateCheckboxInput(session,"update_images",value=TRUE)
    }
    updateCheckboxInput(session,"update_head",value=TRUE)
  })
  observeEvent(mapclicks$particle_locations,{
    updateCheckboxInput(session,"update_particles",value=TRUE)
  })

  observeEvent(input$max_tracking_time_years,{
    if (!is.null(particles_utm())) {
      updateCheckboxInput(session,"update_particles",value=TRUE)
    }
  })

  observeEvent(aquifer(),{
    if (!is.null(wells_utm())) {
      # don't update images -- unless bounds_sf is changed
      updateCheckboxInput(session,"update_head",value=TRUE)
    }
    if (!is.null(particles_utm())) {
      updateCheckboxInput(session,"update_particles",value=TRUE)
    }
  })

  # update checkboxes on results tab if changes on prepare tab
  observeEvent(input$update_images,{
    updateCheckboxInput(session,"update_images_results",value=input$update_images)
  })
  observeEvent(input$update_head,{
    updateCheckboxInput(session,"update_head_results",value=input$update_head)
  })
  observeEvent(input$update_particles,{
    updateCheckboxInput(session,"update_particles_results",value=input$update_particles)
  })

  # if checkbox clicked on results tab, re-run code
  observeEvent(input$update_images_results,{
    if (input$update_images_results & input$maintabs == "results") {
      updateResults$update_images_now = updateResults$update_images_now + 1
    }
  })
  observeEvent(input$update_head_results,{
    if (input$update_head_results & input$maintabs == "results") {
      updateResults$update_head_now = updateResults$update_head_now + 1
    }
  })
  observeEvent(input$update_particles_results,{
    if (input$update_particles_results & input$maintabs == "results") {
      updateResults$update_particles_now = updateResults$update_particles_now + 1
    }
  })

  # map linking
  observeEvent(input$linkmaps,{
    updateCheckboxInput(session,"linkmaps_results",value=input$linkmaps)
  })
  observeEvent(input$linkmaps_results,{
    updateCheckboxInput(session,"linkmaps",value=input$linkmaps_results)
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
           "wells" = "Click a well to edit, or click an empty space to add a well.",
           "particles" = "Particles usermode instr not working")
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
           "wells" = "Define wells",
           "particles" = "Initiate particles")
  })

  output$resultsmaptitle <- renderText({
    "Results"
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

  observeEvent(input$porosity,{
    updateNumericInput(session,"porosity_roi",value=input$porosity)
  })
  observeEvent(input$porosity_roi,{
    updateNumericInput(session,"porosity",value=input$porosity_roi)
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
      input$usermode=="particles" & clickType=="map" ~ "new_particle",
      clickType=="marker" ~ "select_point"
      # input$usermode=="wells" & clickType=="marker" ~ "edit_well",
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
      # leafletProxy("prepmap") %>%
      #   clearGroup("wells") %>%
      #   addCircleMarkers(~x, ~y, color = ~wellPal(selected), group = "wells", opacity = 1, radius = 5,
      #                    data=mapclicks$well_locations)
    } else if (clickOperation == "new_particle") {
      # leafletProxy("prepmap") %>%
      #   clearGroup("wells") %>%
      #   addCircleMarkers(~x, ~y, color = ~wellPal(selected), group = "wells", opacity = 1, radius = 5,
      #                    data=mapclicks$well_locations)
    } else if (clickOperation == "select_point") {
      well_dist <- mapclicks$well_locations %>%
        dplyr::mutate(dist=sqrt((x-markerClick$lng)^2 + (y - markerClick$lat)^2)) %>% dplyr::pull(dist) %>% min()
      particle_dist <- mapclicks$particle_locations %>%
        dplyr::mutate(dist=sqrt((x-markerClick$lng)^2 + (y - markerClick$lat)^2)) %>% dplyr::pull(dist) %>% min()

      selectOperation <- ifelse(well_dist <= particle_dist,"edit_well","edit_particle")

      if (selectOperation == "edit_well") {
        updateTabsetPanel(session,"usermode","wells")
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
      } else if (selectOperation == "edit_particle") {
        updateTabsetPanel(session,"usermode","particles")
        mapclicks$particle_locations <- mapclicks$particle_locations %>%
          dplyr::mutate(dist=sqrt((x-markerClick$lng)^2 + (y - markerClick$lat)^2),
                        selected=dist==min(dist)) %>%
          dplyr::select(-dist)
        leafletProxy("prepmap") %>%
          clearGroup("particles") %>%
          addCircleMarkers(~x, ~y, color = ~partPal(selected), group = "particles", opacity = 1, radius = 5,
                           data=mapclicks$particle_locations)
      }
    }
  })

  # # Map marker click (select well)
  # observeEvent(input$map_marker_click,{
  #   markerClick <- input$map_marker_click
  #   output$current_well <- renderPrint({print(markerClick)})
  # })

  observeEvent(wells_utm(),{
    print('hello')
    print(wells_roi())
    leafletProxy("prepmap") %>%
      clearGroup("wells") %>%
      addPolygons(data=wells_roi() %>% sf::st_transform(4326),fillColor="black",stroke=FALSE, group = "wells") %>%
      addCircleMarkers(~x, ~y, color = ~wellPal(selected), group = "wells", opacity = 1, radius = 5,
                       data=mapclicks$well_locations)
  })

  observeEvent(particles_utm(),{
    print('particles_utm')
    leafletProxy("prepmap") %>%
      clearGroup("particles") %>%
      addCircleMarkers(~x, ~y, color = ~partPal(selected), group = "particles", opacity = 1, radius = 5,
                       data=mapclicks$particle_locations)
  })

  observeEvent(input$deleteWell,{
    mapclicks$well_locations <- mapclicks$well_locations %>%
      dplyr::filter(!selected)
    leafletProxy("prepmap") %>%
      clearGroup("wells") %>%
      addCircleMarkers(~x, ~y, color = ~wellPal(selected), group = "wells",
                       data=mapclicks$well_locations)
  })

  observeEvent(input$deleteParticle,{
    mapclicks$particle_locations <- mapclicks$particle_locations %>%
      dplyr::filter(!selected)
    leafletProxy("prepmap") %>%
      clearGroup("particles") %>%
      addCircleMarkers(~x, ~y, color = ~partPal(selected), group = "particles", opacity = 1, radius = 5,
                       data=mapclicks$particle_locations)
  })

  observeEvent(input$resetMap,{
    mapclicks$bound_vertices <- data.frame(x=numeric(),y=numeric(),bID=integer())
    mapclicks$well_locations <- data.frame(Q=numeric(),R=numeric(),diam=numeric(),group=character(),weight=numeric(),
                              x=numeric(),y=numeric(),wID=integer(),selected=logical())
    mapclicks$particle_locations=data.frame(pID=integer(),x=numeric(),y=numeric(),
                                  selected=logical(),stringsAsFactors = FALSE)
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
                  backgroundColor = styleEqual(c(FALSE,TRUE),c('white','lightgreen')))
  )

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
                  backgroundColor = styleEqual(c(FALSE,TRUE),c('white','lightgreen')))
  )

  output$particletable <- renderDataTable(
    datatable(mapclicks$particle_locations %>% dplyr::select(pID,x,y,selected) %>%
                dplyr::mutate(x=round(x,4),y=round(y,4)),
              rownames=F,#editable=T,
              options = list(searching=FALSE,
                             # formatNumber= function(x) format(x,nsmall=3),
                             lengthChange=FALSE,
                             autoWidth = TRUE,
                             columnDefs = list(list(width = '200px', targets = "_all")))
    ) %>%
      formatStyle('selected',target='row',
                  backgroundColor = styleEqual(c(FALSE,TRUE),c('white','pink')))
  )

  output$particletable_output <- renderDataTable(
    datatable(particles$tracking %>%
                dplyr::mutate(x_end=round(x_end,4),y_end=round(y_end,4),time_years=round(time_years,3)),
              rownames=F,
              options = list(searching=FALSE,
                             # formatNumber= function(x) format(x,nsmall=3),
                             lengthChange=FALSE,
                             autoWidth = TRUE,
                             # columnDefs = list(list(width = '200px', targets = "_all")),
                             scrollX = TRUE
              )
    )
  )

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
    # if (j %in% c(6,7)) { # update map if x or y change
    #   leafletProxy("prepmap") %>%
    #     clearGroup("wells") %>%
    #     addCircleMarkers(~x, ~y, color = ~wellPal(selected), group = "wells",
    #                      data=mapclicks$well_locations)
    # }
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
    updateResults$update_head_now <- updateResults$update_head_now + 1
    updateCheckboxInput(session,"update_head",value=FALSE)
    updateCheckboxInput(session,"update_head_results",value=FALSE)
  })

  view_results <- reactiveValues(
    first_time = TRUE
  )

  observeEvent(input$maintabs,{
    print(input$usermode)
    if (input$maintabs == "results" & input$update_images) {
      updateResults$update_images_now <- updateResults$update_images_now + 1
    } else if (input$maintabs == "results" & input$update_head) {
      updateResults$update_head_now <- updateResults$update_head_now + 1
    } else if (input$maintabs == "results" & input$update_particles) {
      updateResults$update_particles_now <- updateResults$update_particles_now + 1
    }

    # this is needed to set the map extents equal
    if (input$maintabs == "results" & view_results$first_time) {
        prepmapbounds <- input$prepmap_bounds
        leafletProxy("resultsmap") %>%
          fitBounds(prepmapbounds$west,prepmapbounds$south,
                    prepmapbounds$east,prepmapbounds$north,
                    options(animate=FALSE,duration=0))
        view_results$first_time <- FALSE
    }
  })

  observeEvent(input$prepmap_bounds,{
    if (input$linkmaps & input$maintabs == "prepare") { # only update when on prepare tab -- this prevents weird map jiggles
      prepmapbounds <- input$prepmap_bounds
      leafletProxy("resultsmap") %>%
        fitBounds(prepmapbounds$west,prepmapbounds$south,
                  prepmapbounds$east,prepmapbounds$north,
                  options(animate=FALSE,duration=0))
    }
  })

  observeEvent(input$resultsmap_bounds,{
    if (input$linkmaps & input$maintabs == "results") { # only update when on prepare tab -- this prevents weird map jiggles
      resultsmap_bounds <- input$resultsmap_bounds
      leafletProxy("prepmap") %>%
        fitBounds(resultsmap_bounds$west,resultsmap_bounds$south,
                  resultsmap_bounds$east,resultsmap_bounds$north,
                  options(animate=FALSE,duration=0))
    }
  })

  observeEvent(updateResults$update_images_now,{
    print("observeEvent(updateResults$update_images_now)")
    print('nrow mapclicks$well_locations')
    print(nrow(mapclicks$well_locations))

    leafletProxy("resultsmap") %>%
      clearGroup("wells") %>%
      clearGroup("head_cl") %>%
      clearControls()

    withProgress(message="Reproducing aquifer boundaries", value=0, {
      n_progress <- 5
      # print("2")
      bounds_utm <- NULL
      # if there are bounds, map them and get proj4string
      if(!is.null(bounds$bounds_sf)) {
        incProgress(1/n_progress,detail="Getting aquifer properties")
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
        if(!is.null(wells_utm())) {
          aquifer_utm <- aquifer()
        }
      }
      if (!is.null(wells_utm())) {

        incProgress(1/n_progress,detail="Filtering for wells in aquifer")

        # filter for only wells inside the aquifer
        if (!is.null(aquifer_utm$bounds)) {
          bounds_polygon <- use_anem_function("bounds_sf_to_polygon",bounds_sf=aquifer_utm$bounds)
          wells_utm_orig <- wells_utm()
          wells_utm_clipped <- sf::st_intersection(wells_utm(),bounds_polygon)
          if (!identical(wells_utm_clipped,wells_utm_orig)) {
            warning(dim(wells_utm_orig)[1]-dim(wells_utm_clipped)[1]," wells were outside the aquifer boundary. They have been removed.")
            mapclicks$well_locations <- mapclicks$well_locations %>% dplyr::filter(wID %in% wells_utm_clipped$wID)
          }
        }

        # generate well images
        incProgress(1/n_progress,detail="Generating well images")
        wells$utm_with_images <- wells_utm_clipped %>%
          define_wells() %>%
          generate_image_wells(aquifer_utm)
        # print("n wells")
        # print(wells$utm_with_images)
        incProgress(1/n_progress,detail="Mapping the wells")
        leafletProxy("resultsmap") %>%
          clearGroup("wells") %>%
          addCircleMarkers(~x, ~y, color = ~wellPal(selected), group = "wells",
                           data=mapclicks$well_locations)
        # print("7")
      }
    })
    updateResults$update_head_now <- updateResults$update_head_now + 1
    updateCheckboxInput(session,"update_images",value=FALSE)
    updateCheckboxInput(session,"update_images_results",value=FALSE)
  })

  observeEvent(updateResults$update_head_now,{
    n_progress <- 5
    print("observeEvent(updateResults$update_head_now)")
    withProgress(message="Generating head",value=0 , {
      aquifer_utm <- aquifer()
      bounds_utm <- NULL
      if(!is.null(bounds$bounds_sf)) {
        incProgress(1/n_progress,detail="Converting bounds to UTM")
        bounds_utm <- bounds$bounds_sf %>%
          dplyr::select(-dplyr::matches('^[mb]$'),-dplyr::matches("[xy][12]")) %>%
          sf::st_transform(anem::utm_zone_to_proj4(utm_zone()))
        aquifer_utm$bounds <- define_bounds(bounds_utm)
      }
      if (!is.null(wells$utm_with_images)) {
        # print('1')
        # print('aquifer_utm')
        # print(aquifer_utm)


        incProgress(1/n_progress,detail="Getting head at wells")
        # get head at wells
        head_wells <- wells$utm_with_images %>% dplyr::filter(wID==orig_wID) %>%
          anem::get_hydraulic_head(wells$utm_with_images,aquifer_utm)
        # print(head_wells)
        wells$head <- wells$utm_with_images %>% dplyr::filter(wID==orig_wID) %>%
          dplyr::select(wID) %>% sf::st_set_geometry(NULL) %>%
          dplyr::mutate(`Head, m`=round(head_wells,3),
                        `Drawdown, m`=round(input$h0-`Head, m`,3))
        # print("wells$head")
        # print(wells$head)
        replaceData(proxy_welltable_head, wells$head, resetPaging = FALSE, rownames = F)

        # get gridded head
        incProgress(1/n_progress,detail="Getting gridded head in aquifer")
        gridded$h <- anem::get_gridded_hydrodynamics(wells$utm_with_images,aquifer_utm,head_dim=c(100,100),flow_dim=c(5,5))
        # gridded$raster_utm <- rasterFromXYZ(gridded$h$head %>% dplyr::rename(z=head_m),crs=proj4string_scenario())
        # gridded$raster_wgs <-  gridded$raster_utm %>% projectRaster(crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

        bounds_polygon <- use_anem_function("bounds_sf_to_polygon",bounds_sf=aquifer_utm$bounds)

        # get gridded head as polygons
        # head_sf_wgs <- raster::rasterToPolygons(gridded$raster_wgs,dissolve = TRUE) %>%
        #   sf::st_as_sf() %>% sf::st_intersection(bounds_polygon %>% sf::st_transform(4326)) %>%
        #   dplyr::rename(head_m=z) %>% dplyr::mutate(head_label=paste0("Head: ",round(head_m,2)," m"))
        # print(head_sf_wgs)


        # get contour lines of head
        incProgress(1/n_progress,detail="Getting head contours")
        cl <- get_contourlines(gridded$h$head %>% dplyr::rename(z=head_m),
                               type="sf",crs=proj4string_scenario())
        cl <- cl %>% sf::st_intersection(bounds_polygon)

        # head_range <- c(min(gridded$h$head_m),max(gridded$h$head_m))
        incProgress(1/n_progress,detail="Mapping results")
        headPal <- colorNumeric(palette = "Blues",domain=cl$level, reverse=TRUE)
        headPal_rev <- colorNumeric(palette = "Blues",domain=cl$level)
        leafletProxy("resultsmap") %>%
          clearGroup("head_cl") %>%
          clearControls() %>%
          addPolylines(color=~headPal(level),opacity=1,weight=3, group="head_cl",# label=~as.character(head_label),
                       data=cl %>% sf::st_transform(crs=4326)) %>% # %>%
          # addPolygons(data=head_sf_wgs,stroke=FALSE,fillOpacity = 0,label=~as.character(head_label)) %>%
          addLegend("bottomright", pal = headPal_rev, values = ~level, group="head_cl",
                    title = "Head, m", data=cl,
                    labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)),
                    opacity = 1 )

        wells$drawdown_relationships <- get_drawdown_relationships(wells$utm_with_images,aquifer(),group_column = Group, weights_column = Weight)

        updateResults$update_particles_now <- updateResults$update_particles_now + 1
      }
    })
    updateCheckboxInput(session,"update_head",value=FALSE)
    updateCheckboxInput(session,"update_head_results",value=FALSE)
    if (!is.null(notification_id)) {
      removeNotification(notification_id)
    }
    notification_id <<- NULL

    if (input$aquifer_type == "unconfined" & min(wells$head$`Head, m`) <= 0 & nrow(mapclicks$well_locations) > 0) {
      notification_id <<- showNotification("Warning: results invalid, fully depleted aquifer",duration=NULL,type="error")
    }
  })

  observeEvent(updateResults$update_particles_now,{
    print("particles 1")
    if (!is.null(particles_utm()) & !is.null(bounds$bounds_sf) & !is.null(wells_utm())) {
      # prep for particle tracking
      print("particles 2")
      particle_pIDs <- particles_utm() %>% dplyr::pull(pID)
      particles_matrix <- particles_utm() %>% sf::st_set_geometry(NULL) %>%
        dplyr::select(x,y) %>% as.matrix()
      n_progress <- nrow(particles_matrix) + 2

      shiny::withProgress(message="Particle tracking",value=0,{
        incProgress(1/n_progress,detail="Getting aquifer properties")
        # get aquifer boundaries
        bounds_utm <- bounds$bounds_sf %>%
          dplyr::select(-dplyr::matches('^[mb]$'),-dplyr::matches("[xy][12]")) %>%
          sf::st_transform(anem::utm_zone_to_proj4(utm_zone()))
        aquifer_utm <- aquifer()
        aquifer_utm$bounds <- define_bounds(bounds_utm)
        print("particles 3")

        print(aquifer_utm)

        for (i in 1:nrow(particles_utm())) {
          print("particles 4")
          incProgress(1/n_progress,detail=paste0("pID = ",particle_pIDs[i]))
          loc <- particles_matrix[i,c("x","y")]
          print("particles 5")
          particle <- track_particle(loc,wells$utm_with_images,aquifer_utm,t_max=input$max_tracking_time_years*365)  %>%
            dplyr::mutate(pID=particle_pIDs[i])
          print("particles 6")
          if (i == 1) {
            particle_path_df <- particle
            particle_endpoint <- particle[nrow(particle),]
          } else {
            particle_path_df <- particle_path_df %>% dplyr::bind_rows(particle)
            particle_endpoint <- particle_endpoint %>% dplyr::bind_rows(particle[nrow(particle),])
          }
        }

        incProgress(1/n_progress,detail="Generating polylines")
        particles$paths_wgs <- particle_path_df %>% sf::st_as_sf(coords=c("x","y"),crs=proj4string_scenario()) %>%
          dplyr::group_by(pID) %>% dplyr::summarize(do_union=FALSE) %>%
          sf::st_cast("MULTILINESTRING") %>%
          sf::st_transform(crs=4326)

        print(particles$paths_wgs)

        leafletProxy("resultsmap") %>%
          clearGroup("particles") %>%
          addPolylines(data=particles$paths_wgs,color = "red",group = "particles") %>%
            addCircleMarkers(~x, ~y, color = ~partPal(selected), group = "particles", opacity = 1, radius = 5,
                             data=mapclicks$particle_locations)

        particle_sf <- particle_endpoint %>% sf::st_as_sf(coords=c("x","y"),crs=proj4string_scenario()) %>%
          sf::st_transform(crs=4326)
        particle_coords_wgs <- particle_sf %>% sf::st_coordinates() %>% tibble::as_tibble()
        particle_wgs <- particle_sf %>% sf::st_set_geometry(NULL) %>% dplyr::bind_cols(particle_coords_wgs)

        particles$tracking <- data.frame(pID=particle_pIDs) %>%
          dplyr::mutate(time_years=particle_wgs$time_days/365,
                        status=particle_wgs$status,x_end=particle_wgs$X,
                        y_end=particle_wgs$Y)
      })
    }

    updateCheckboxInput(session,"update_particles",value=FALSE)
    updateCheckboxInput(session,"update_particles_results",value=FALSE)
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

  output$wells <- renderPrint({print(mapclicks$particle_locations) %>% tibble::as_tibble()})
  output$bounds <- renderPrint({print(bounds$bounds_sf)})
  # output$clickbounds_rect <- renderPrint({print(bounds$edges_rectangular)})
  # output$clickwells <- renderPrint({print(mapclicks$well_locations)})
  output$aquifer <- renderPrint({print(aquifer())})

  output$drawdowntable <- renderTable({
    wells$drawdown_relationships
  })
}

shinyApp(ui, server)
