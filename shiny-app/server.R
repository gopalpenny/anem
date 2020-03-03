library(leaflet)
library(anem)
library(DT)
library(ggplot2)
library(shinycssloaders)
library(raster)
library(mapview)
library(deSolve)
library(akima)
# library(tibble)

source("app-functions/anem_shiny_helpers.R")

well_group_vals <- LETTERS[1:5]
wellPal <- colorFactor(palette = c("darkgreen","green"), domain = c(FALSE, TRUE))
wellPal2 <- colorFactor(palette = colorspace::rainbow_hcl(n=5,c=100,l=65), domain = well_group_vals)
partPal <- colorFactor(palette = c("darkred","red"), domain = c(FALSE, TRUE))
# opacityFun <- function(x) switch(x+1,0.4,0.8)
boundPal <- colorFactor(palette = c("blue","black","gray"), domain = c("NF", "CH", "PB"))

server <- function(input, output, session) {
  notification_id <- NULL
  note_id_R_diam <- NULL

  output$fileDownload <- downloadHandler(
    filename = function() {
      "anem_scenario.rds"
    },
    content = function(file) {
      saveRDS(list(b1_type=input$b1_type,b2_type=input$b2_type,b3_type=input$b3_type,b4_type=input$b4_type,
                   aquifer_type=input$aquifer_type,porosity=input$porosity,Ksat=input$Ksat,h0=input$h0,z0=input$z0,
                   wellCapture=input$wellCapture,captureParticles=input$captureParticles,
                   enableRecharge=input$enableRecharge,rechargeFlow=input$rechargeFlow,max_tracking_time_years=input$max_tracking_time_years,
                   bound_vertices=mapclicks$bound_vertices, particle_locations=mapclicks$particle_locations,
                   recharge_vertices=mapclicks$recharge_vertices, well_locations=mapclicks$well_locations), file)
    }
  )

  fileUp <- reactive({
    # print("FILE UPLOAD BEGINS")
    req(input$fileUpload)

    params_list <- readRDS(input$fileUpload$datapath)
    params_list
  })

  output$printfile <- renderPrint({
    print(fileUp())
  })

  observeEvent(fileUp(),{
    pl <- fileUp()
    updateSelectInput(session,"b1_type",selected=pl$b1_type)
    updateSelectInput(session,"b2_type",selected=pl$b2_type)
    updateSelectInput(session,"b3_type",selected=pl$b3_type)
    updateSelectInput(session,"b4_type",selected=pl$b4_type)
    updateSelectInput(session,"aquifer_type",selected=pl$aquifer_type)

    updateNumericInput(session,"porosity",value=pl$porosity)
    updateNumericInput(session,"Ksat",value=pl$Ksat)
    updateNumericInput(session,"h0",value=pl$h0)
    updateNumericInput(session,"z0",value=pl$z0)
    updateNumericInput(session,"rechargeFlow",value=pl$rechargeFlow)
    updateNumericInput(session,"max_tracking_time_years",value=pl$max_tracking_time_years)
    updateNumericInput(session,"z0",value=pl$z0)

    updateCheckboxInput(session,"wellCapture",value=pl$wellCapture)
    updateSliderInput(session,"captureParticles",value=pl$captureParticles)

    updateCheckboxInput(session,"enableRecharge",value=pl$enableRecharge)

    mapclicks$bound_vertices <- pl$bound_vertices
    mapclicks$particle_locations <- pl$particle_locations
    mapclicks$recharge_vertices <- pl$recharge_vertices
    mapclicks$well_locations <- pl$well_locations

    print("mapclicks$well_locations")
    print(mapclicks$well_locations)

    updateResults$reset_zoom <- updateResults$reset_zoom + 1
    shiny::updateActionButton(session,"resetZoomLink")
  })

  # Check when to update results
  updateResults <- reactiveValues(
    wells_next_view=TRUE,
    particles_next_view=TRUE,
    update_images_now = 0,
    update_head_now = 0,
    # update_particle_tracking
    update_particles_now = 0,
    reset_zoom = 0
  )

  # Initialize reactive values
  mapclicks <- reactiveValues(
    bound_vertices=data.frame(bID=integer(),#bound_type=factor(levels=c("NF","CH")),
                              x=numeric(),y=numeric()),
    recharge_vertices=data.frame(rID=integer(),x=numeric(),y=numeric()),
    well_locations=data.frame(Q=numeric(),R=numeric(),diam=numeric(),Group=character(),Weight=numeric(),
                              x=numeric(),y=numeric(),wID=integer(),selected=logical(),stringsAsFactors = FALSE),
    particle_locations=data.frame(pID=integer(),x=numeric(),y=numeric(),
                                  selected=logical(),stringsAsFactors = FALSE)
  )

  recharge_params <- reactive({
    if (nrow(mapclicks$recharge_vertices) == 2 & !is.null(proj4string_scenario()) & input$enableRecharge) {
      rv <- mapclicks$recharge_vertices %>%
        sf::st_as_sf(coords=c("x","y"),crs=4326) %>%
        sf::st_transform(crs=proj4string_scenario()) %>%
        dplyr::bind_cols(sf::st_coordinates(.) %>% tibble::as_tibble() %>% setNames(c("x","y"))) %>%
        sf::st_set_geometry(NULL)
      recharge_params <- list(recharge_vector=c(rv$x[1],rv$y[1],rv$x[2],rv$y[2]),
                              x0=rv$x[1], y0=rv$y[1],
                              recharge_type = "F",
                              flow = input$rechargeFlow)
    } else {
      recharge_params <- NULL
    }
    recharge_params
  })

  output$recharge_df <- renderPrint({
    print(aquifer())
  })

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
    bounds=bounds$bounds_sf,
    recharge=recharge_params())})

  wells <- reactiveValues(
    utm_with_images = NULL,
    head=tibble::tibble(wID=NA,
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
    particle_paths_wgs = NULL,
    capture_paths_wgs = NULL,
    capture_endpoints = NULL,
    tracking = tibble::tibble(pID=integer(),time_years=numeric(),status=character(),x_end=numeric(),y_end=numeric())
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
    # if (!is.null(particles_utm()) | input$wellCapture) {
    #   updateCheckboxInput(session,"update_particles",value=TRUE)
    # }
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
    if (!is.null(particles_utm()) | input$wellCapture) {
      updateCheckboxInput(session,"update_particles",value=TRUE)
    }
  })
  observeEvent(input$wellCapture,{
    if (input$wellCapture) {
      updateCheckboxInput(session,"update_particles",value=TRUE)
    } else if (is.null(particles_utm())) {
      updateCheckboxInput(session,"update_particles",value=FALSE)
    }
  })

  observeEvent(input$captureParticles,{
    if (input$wellCapture) {
      updateCheckboxInput(session,"update_particles",value=TRUE)
    }
  })

  observeEvent(aquifer(),{
    if (!is.null(wells_utm())) {
      # don't update images -- unless bounds_sf is changed
      updateCheckboxInput(session,"update_head",value=TRUE)
    }
    if (!is.null(particles_utm()) | input$wellCapture) {
      updateCheckboxInput(session,"update_particles",value=TRUE)
    }
  })

  # update checkboxes on results tab if changes on prepare tab
  observeEvent(input$update_images,{
    updateCheckboxInput(session,"update_images_results",value=input$update_images)
  })
  observeEvent(input$update_head,{
    updateCheckboxInput(session,"update_head_results",value=input$update_head)
    if (!is.null(particles_utm()) | input$wellCapture) {
      updateCheckboxInput(session,"update_particles",value=input$update_head)
    }
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
           "confined" = round(sqrt(2.25*Ksat*h0*input$pumpingtime_months*30*24*3600/input$storativity)), #pumpingtime_months to seconds
           "unconfined"=round(sqrt(1.90*Ksat*h0*input$pumpingtime_months*30*24*3600/input$porosity)))
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
           "particles" = "Initiate particles",
           "files" = "Save or upload")
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
      input$usermode=="aquifer" & input$aquifermode == "boundaries" ~ "aquifer_vertex",
      input$usermode=="aquifer" & input$aquifermode == "recharge" ~ "recharge_vertex",
      input$usermode=="wells" & clickType=="map" ~ "new_well",
      input$usermode=="particles" & clickType=="map" ~ "new_particle",
      clickType=="marker" ~ "select_point",
      TRUE~"none"
      # input$usermode=="wells" & clickType=="marker" ~ "edit_well",
    )
    well_input <- list(Q=input$Q,R=newwellROI(),diam=input$diam,group=input$well_group,weight=input$well_weight)
    mapclicks <- interpret_map_click(prepmapClick,clickOperation,mapclicks,well_input=well_input)
    if (clickOperation == "select_point") {
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
      } else if (selectOperation == "edit_particle") {
        updateTabsetPanel(session,"usermode","particles")
        mapclicks$particle_locations <- mapclicks$particle_locations %>%
          dplyr::mutate(dist=sqrt((x-markerClick$lng)^2 + (y - markerClick$lat)^2),
                        selected=dist==min(dist)) %>%
          dplyr::select(-dist)
      }
    }
  })

  observeEvent(mapclicks$bound_vertices,{
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
  })

  observeEvent(mapclicks$recharge_vertices,{
    leafletProxy("prepmap",data=mapclicks$recharge_vertices[1,]) %>%
      clearGroup("rechargevertices") %>%
      addCircleMarkers(~x, ~y, color = "blue", group = "rechargevertices", opacity = 0.5, radius = 7)
    if (nrow(mapclicks$recharge_vertices) > 1) {
      leafletProxy("prepmap",data=mapclicks$recharge_vertices) %>%
        addCircleMarkers(~x, ~y, color = "blue", group = "rechargevertices", opacity = 0.5, radius = 2) %>%
        addPolylines(~x, ~y, color = "blue", group = "rechargevertices", opacity = 0.3, weight = 2,
                     layerId = "rechargevector",fillOpacity = 0)

    }
  })

  observeEvent(mapclicks$well_locations,{
    if (!is.null(note_id_R_diam)) {
      removeNotification(note_id_R_diam)
      note_id_R_diam <<- NULL
    }
    if ((any(mapclicks$well_locations$R) <= 0 |
        any(mapclicks$well_locations$diam <= 0) |
        any(mapclicks$well_locations$R <= mapclicks$well_locations$diam)) & nrow(mapclicks$well_locations) > 0) {
      id <- showNotification("For each well, R and diam must be nonzero with R > diam.",type="warning",duration = 20)
      note_id_R_diam <<- id
    }
    leafletProxy("prepmap") %>%
      clearGroup("Wells") %>% clearControls() %>%
      addCircleMarkers(~x, ~y, color = ~wellPal2(Group), group = "Wells", opacity = 1, radius = 5,
                       data=mapclicks$well_locations) %>%
      addCircleMarkers(~x, ~y, color = ~wellPal2(Group), group = "Wells", opacity = 0.5, radius = 10,
                       data=mapclicks$well_locations %>% dplyr::filter(selected)) %>%
      addLegend(pal = wellPal2, values= ~Group, group = "Wells", data=mapclicks$well_locations,position="bottomright")
  })

  observeEvent(mapclicks$particle_locations,{
    leafletProxy("prepmap") %>%
      clearGroup("Particles") %>%
      addCircleMarkers(~x, ~y, color = ~partPal(selected), group = "Particles", opacity = 1, radius = 5,
                       data=mapclicks$particle_locations)
  })
    # # print(mapclicks$well_locations %>% tibble::as_tibble())
    # if (clickOperation == "aquifer_vertex") {
    # } else if (clickOperation == "recharge_vertex") {
    # } else if (clickOperation == "new_well") {
    #   # leafletProxy("prepmap") %>%
    #   #   clearGroup("Wells") %>%
    #   #   addCircleMarkers(~x, ~y, color = ~wellPal(selected), group = "Wells", opacity = 1, radius = 5,
    #   #                    data=mapclicks$well_locations)
    # } else if (clickOperation == "new_particle") {
    #   # leafletProxy("prepmap") %>%
    #   #   clearGroup("Wells") %>%
    #   #   addCircleMarkers(~x, ~y, color = ~wellPal(selected), group = "Wells", opacity = 1, radius = 5,
    #   #                    data=mapclicks$well_locations)
    # } else

  # # Map marker click (select well)
  # observeEvent(input$map_marker_click,{
  #   markerClick <- input$map_marker_click
  #   output$current_well <- renderPrint({print(markerClick)})
  # })

  observeEvent(wells_utm(),{
    print('hello')
    print(wells_roi())
    leafletProxy("prepmap") %>%
      clearGroup("Wells") %>% clearControls() %>%
      addPolygons(data=wells_roi() %>% sf::st_transform(4326),fillColor="black",fillOpacity = 0.07,opacity=0.4,stroke=TRUE,color="#888888", weight=1, group = "Wells") %>%
      addCircleMarkers(~x, ~y, color = ~wellPal2(Group), group = "Wells", opacity = 1, radius = 5,
                       data=mapclicks$well_locations) %>%
      addCircleMarkers(~x, ~y, color = ~wellPal2(Group), group = "Wells", opacity = 0.5, radius = 10,
                       data=mapclicks$well_locations %>% dplyr::filter(selected)) %>%
      addLegend(pal = wellPal2, values= ~Group, group = "Wells", data=mapclicks$well_locations, position="bottomright") #%>%
    # addLayersControl(baseGroups = c("Map","Satellite"),
    #                  overlayGroups = c("Wells"),
    #                  options = layersControlOptions(collapsed=FALSE))
  })

  observeEvent(particles_utm(),{
    print('particles_utm')
    leafletProxy("prepmap") %>%
      clearGroup("Particles") %>%
      addCircleMarkers(~x, ~y, color = ~partPal(selected), group = "Particles", opacity = 1, radius = 5,
                       data=mapclicks$particle_locations)
  })

  observeEvent(input$deleteWell,{
    mapclicks$well_locations <- mapclicks$well_locations %>%
      dplyr::filter(!selected)
    leafletProxy("prepmap") %>%
      clearGroup("Wells") %>% clearControls() %>%
      addPolygons(data=wells_roi() %>% sf::st_transform(4326),fillColor="black",fillOpacity = 0.07,opacity=0.4,stroke=TRUE,color="#888888", weight=1, group = "Wells") %>%
      addCircleMarkers(~x, ~y, color = ~wellPal2(Group), group = "Wells",
                       data=mapclicks$well_locations) %>%
      addCircleMarkers(~x, ~y, color = ~wellPal2(Group), group = "Wells", opacity = 0.5, radius = 10,
                       data=mapclicks$well_locations %>% dplyr::filter(selected)) %>%
      addLegend(pal = wellPal2, values= ~Group, group = "Wells", data=mapclicks$well_locations, position="bottomright")
  })

  observeEvent(input$deleteParticle,{
    mapclicks$particle_locations <- mapclicks$particle_locations %>%
      dplyr::filter(!selected)
    leafletProxy("prepmap") %>%
      clearGroup("Particles") %>%
      addCircleMarkers(~x, ~y, color = ~partPal(selected), group = "Particles", opacity = 1, radius = 5,
                       data=mapclicks$particle_locations)
  })

  observeEvent(input$clearMap,{
    mapclicks$bound_vertices <- data.frame(x=numeric(),y=numeric(),bID=integer())
    mapclicks$recharge_vertices <- data.frame(rID=integer(),x=numeric(),y=numeric())
    mapclicks$well_locations <- data.frame(Q=numeric(),R=numeric(),diam=numeric(),group=character(),weight=numeric(),
                                           x=numeric(),y=numeric(),wID=integer(),selected=logical())
    mapclicks$particle_locations=data.frame(pID=integer(),x=numeric(),y=numeric(),
                                            selected=logical(),stringsAsFactors = FALSE)
    leafletProxy("prepmap") %>%
      clearShapes() %>% clearMarkers()
    leafletProxy("resultsmap") %>%
      clearShapes() %>% clearMarkers()
  })

  observeEvent(input$resetZoomLink,{
    updateResults$reset_zoom <- updateResults$reset_zoom + 1
  })
  observeEvent(input$resetZoomLink_results,{
    updateResults$reset_zoom <- updateResults$reset_zoom + 1
  })

  observeEvent(updateResults$reset_zoom,{
    x1 <- min(c(mapclicks$bound_vertices$x,mapclicks$well_locations$x,Inf)) - 0.01
    x2 <- max(c(mapclicks$bound_vertices$x,mapclicks$well_locations$x,-Inf)) + 0.01
    y1 <- min(c(mapclicks$bound_vertices$y,mapclicks$well_locations$y,Inf)) - 0.01
    y2 <- max(c(mapclicks$bound_vertices$y,mapclicks$well_locations$y,-Inf)) + 0.01
    print(mapclicks$bound_vertices)
    print(mapclicks$well_locations)
    if (all(abs(c(x1, y1, x2, y2)) != Inf)) {
      leafletProxy("prepmap") %>%
        fitBounds(x1, y1, x2, y2)
      leafletProxy("resultsmap") %>%
        fitBounds(x1, y1, x2, y2)
    }
  })

  # observeEvent(input$resetZoom_results,{
  #   x1 <- min(c(mapclicks$bound_vertices$x,mapclicks$well_locations$x)) - 0.01
  #   x2 <- max(c(mapclicks$bound_vertices$x,mapclicks$well_locations$x)) + 0.01
  #   y1 <- min(c(mapclicks$bound_vertices$y,mapclicks$well_locations$y)) - 0.01
  #   y2 <- max(c(mapclicks$bound_vertices$y,mapclicks$well_locations$y)) + 0.01
  #   print(mapclicks$bound_vertices)
  #   print(mapclicks$well_locations)
  #   leafletProxy("prepmap") %>%
  #     fitBounds(x1, y1, x2, y2)
  # })

  output$welltable <- renderDataTable(
    datatable(mapclicks$well_locations,
              editable=T,rownames=F,
              options = list(searching=FALSE,
                             # formatNumber= function(x) format(x,nsmall=3),
                             lengthChange=FALSE,
                             autoWidth = TRUE#,
                             # columnDefs = list(list(width = '200px', targets = "_all"))
              )
    ) %>%
      formatStyle('selected',target='row',
                  backgroundColor = styleEqual(c(FALSE,TRUE),c('white','lightgray')))
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
                  backgroundColor = styleEqual(c(FALSE,TRUE),c('white','lightgray')))
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
    new_value <- DT::coerceValue(v, mapclicks$well_locations[i, j])
    if (names(mapclicks$well_locations)[j] != "Group") {
      mapclicks$well_locations[i, j] <<- new_value
    } else if (names(mapclicks$well_locations)[j] == "Group" & new_value %in% well_group_vals) {
      mapclicks$well_locations[i, j] <<- new_value
    } else {
      print(paste("Attempted to assign well group value of",new_value))
      err_msg <- paste("Well Group value must be one of",
                       paste(well_group_vals,collapse=", "))
      showNotification(err_msg,duration = 10,type="warning")
    }
    replaceData(proxy_welltable, mapclicks$well_locations, resetPaging = FALSE, rownames = F)
    replaceData(proxy_welltable2, mapclicks$well_locations %>% dplyr::select(Q,diam,Group,selected), resetPaging = FALSE, rownames = F)
    # if (j %in% c(6,7)) { # update map if x or y change
    #   leafletProxy("prepmap") %>%
    #     clearGroup("Wells") %>%
    #     addCircleMarkers(~x, ~y, color = ~wellPal(selected), group = "Wells",
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
      clearGroup("Wells") %>%
      clearGroup("Head") %>%
      clearControls()

    print("images 1")
    withProgress(message="Reproducing aquifer boundaries", value=0, {
      n_progress <- 5
      print("images 2")
      bounds_utm <- NULL
      # if there are bounds, map them and get proj4string
      if(!is.null(bounds$bounds_sf)) {
        incProgress(1/n_progress,detail="Getting aquifer properties")
        bounds_utm <- bounds$bounds_sf %>%
          dplyr::select(-dplyr::matches('^[mb]$'),-dplyr::matches("[xy][12]")) %>%
          sf::st_transform(anem::utm_zone_to_proj4(utm_zone()))
        print("images 2.1")
        aquifer_utm <- aquifer()
        print("images 2.2")
        # saveRDS(bounds_utm,"app-debug/bounds_utm.rds")
        print("images 2.3")
        aquifer_utm$bounds <- define_bounds(bounds_utm)
        print("images 2.4")
        # print(bounds$bounds_sf)
        leafletProxy("resultsmap") %>%
          clearGroup("bounds_rectangular") %>%
          clearControls() %>%
          addPolylines(color = ~boundPal(bound_type), group = "bounds_rectangular",
                       fillOpacity = 0, opacity = 1, weight = 4,
                       data=bounds$bounds_sf)
        print('images 2.5')
        # if no bounds, get proj4string from wells
      } else {
        print('images 2.9')
        if(!is.null(wells_utm())) {
          aquifer_utm <- aquifer()
        }
      }
      if (!is.null(wells_utm())) {
        print('images 3')

        incProgress(1/n_progress,detail="Filtering for wells in aquifer")

        if (!is.null(aquifer_utm$bounds)) {
          print('images 3.1')

          # filter for only wells inside the aquifer
          bounds_polygon <- use_anem_function("bounds_sf_to_polygon",bounds_sf=aquifer_utm$bounds)
          wells_utm_orig <- wells_utm()
          wells_utm_clipped <- sf::st_intersection(wells_utm(),bounds_polygon)
          if (!identical(wells_utm_clipped,wells_utm_orig)) {
            warning(dim(wells_utm_orig)[1]-dim(wells_utm_clipped)[1]," wells were outside the aquifer boundary. They have been removed.")
            mapclicks$well_locations <- mapclicks$well_locations %>% dplyr::filter(wID %in% wells_utm_clipped$wID)
          }

          # generate well images
          print('images 3.2')
          incProgress(1/n_progress,detail="Generating well images")
          wells$utm_with_images <- wells_utm_clipped %>%
            define_wells() %>%
            generate_image_wells(aquifer_utm)

        } else {
          print('images 3.8')
          wells_utm_clipped <- wells_utm()
        }

        incProgress(1/n_progress,detail="Mapping the wells")
        print('images 4')
        leafletProxy("resultsmap") %>%
          clearGroup("Wells") %>%
          addCircleMarkers(~x, ~y, color = ~wellPal2(Group), group = "Wells", opacity = 1, radius = 5,
                           data=mapclicks$well_locations) %>%
          addCircleMarkers(~x, ~y, color = ~wellPal2(Group), group = "Wells", opacity = 0.5, radius = 10,
                           data=mapclicks$well_locations %>% dplyr::filter(selected))
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
        cl_wgs <- cl %>% sf::st_transform(crs=4326)

        # head_range <- c(min(gridded$h$head_m),max(gridded$h$head_m))
        incProgress(1/n_progress,detail="Mapping results")
        headPal <- colorNumeric(palette = "Blues",domain=cl$level, reverse=TRUE)
        headPal_rev <- colorNumeric(palette = "Blues",domain=cl$level)
        leafletProxy("resultsmap") %>%
          clearGroup("Head") %>%
          clearControls() %>%
          addPolylines(color=~headPal(level),opacity=1,weight=3, group="Head",# label=~as.character(head_label),
                       data=cl_wgs) %>% # %>%
          # addPolygons(data=head_sf_wgs,stroke=FALSE,fillOpacity = 0,label=~as.character(head_label)) %>%
          addLegend("bottomright", pal = headPal_rev, values = ~level, group="Head",
                    title = "Head, m", data=cl,
                    labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)),
                    opacity = 1 ) %>%
          addLayersControl(baseGroups = c("Map","Satellite"),
                           overlayGroups = c("Head"),
                           options = layersControlOptions(collapsed=FALSE))

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
    if (!is.null(bounds$bounds_sf) & !is.null(wells_utm())) {
      print("input$wellCapture")
      print(input$wellCapture)

      n_progress <- 3 #!is.null(particles_utm()) * 2 + input$wellCapture * 2 + 1

      shiny::withProgress(message="Particle tracking",value=0,{
        incProgress(1/n_progress,detail="Getting aquifer properties")

        # get aquifer boundaries
        bounds_utm <- bounds$bounds_sf %>%
          dplyr::select(-dplyr::matches('^[mb]$'),-dplyr::matches("[xy][12]")) %>%
          sf::st_transform(anem::utm_zone_to_proj4(utm_zone()))
        aquifer_utm <- aquifer()
        aquifer_utm$bounds <- define_bounds(bounds_utm)
        print(aquifer_utm)

        if (!is.null(particles_utm()) & !input$wellCapture) {
          # prep for particle tracking
          # particle_pIDs <- particles_utm() %>% dplyr::pull(pID)
          particles_df <- particles_utm() %>% sf::st_set_geometry(NULL) %>% tibble::as_tibble()

          incProgress(1/n_progress,detail="Tracking individual particles")
          particle_paths_df <- track_particles(particles_df,wells$utm_with_images,aquifer_utm,t_max=input$max_tracking_time_years*365)
          particle_endpoints <- particle_paths_df %>% dplyr::filter(endpoint)

          incProgress(1/n_progress,detail="Generating individual polylines")
          particles$particle_paths_wgs <- particle_paths_df %>% sf::st_as_sf(coords=c("x","y"),crs=proj4string_scenario()) %>%
            dplyr::group_by(pID) %>% dplyr::summarize(do_union=FALSE) %>%
            sf::st_cast("MULTILINESTRING") %>%
            sf::st_transform(crs=4326)

          leafletProxy("resultsmap") %>%
            clearGroup("Particles") %>%
            clearGroup("Well capture") %>%
            addPolylines(data=particles$particle_paths_wgs,color = "red",group = "Particles", weight = 2) %>%
            addCircleMarkers(~x, ~y, color = ~partPal(selected), group = "Particles", opacity = 1, radius = 5,
                             data=mapclicks$particle_locations) %>%
            addLayersControl(baseGroups = c("Map","Satellite"),
                             overlayGroups = c("Head","Particles"),
                             options = layersControlOptions(collapsed=FALSE))

          particle_sf <- particle_endpoints %>% sf::st_as_sf(coords=c("x","y"),crs=proj4string_scenario()) %>%
            sf::st_transform(crs=4326)
          particle_coords_wgs <- particle_sf %>% sf::st_coordinates() %>% tibble::as_tibble()
          particle_wgs <- particle_sf %>% sf::st_set_geometry(NULL) %>% dplyr::bind_cols(particle_coords_wgs)

          particles$tracking <- particle_wgs %>% dplyr::mutate(time_years=time_days/365) %>%
            dplyr::select(pID,time_years,status,x_end=X,y_end=Y)
        }
        if (input$wellCapture) {
          incProgress(1/n_progress,detail="Well capture zones - tracking")
          capture_paths_df <- get_capture_zone(wells$utm_with_images,aquifer_utm,t_max=input$max_tracking_time_years*365,n_particles = input$captureParticles)

          print("capture_paths_df")
          print(capture_paths_df)

          incProgress(1/n_progress,detail="Generating well capture polylines")
          particles$capture_paths_wgs <- capture_paths_df %>% sf::st_as_sf(coords=c("x","y"),crs=proj4string_scenario()) %>%
            dplyr::group_by(wID,pID) %>% dplyr::summarize(do_union=FALSE) %>%
            sf::st_cast("MULTILINESTRING") %>%
            sf::st_transform(crs=4326)

          particles$capture_endpoints <- capture_paths_df %>% dplyr::filter(endpoint)

          print("particles$capture_paths_wgs")
          print(particles$capture_paths_wgs)
          leafletProxy("resultsmap") %>%
            clearGroup("Particles") %>%
            clearGroup("Well Capture") %>%
            addPolylines(data=particles$capture_paths_wgs,color = "red",group = "Well capture", weight = 2) %>%
            addLayersControl(baseGroups = c("Map","Satellite"),
                             overlayGroups = c("Head","Well capture"),
                             options = layersControlOptions(collapsed=FALSE))

        }
      })
    }

    updateCheckboxInput(session,"update_particles",value=FALSE)
    updateCheckboxInput(session,"update_particles_results",value=FALSE)
  })

  output$capture_endpoint <- renderPrint({
    print(particles$capture_endpoints)
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
