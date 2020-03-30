library(shiny)
library(leaflet)
library(htmlwidgets)

# leaflet examples help page:
# https://rstudio.github.io/leaflet/shiny.html

# CODE IS STARTED FROM HERE:
# https://www.r-bloggers.com/4-tricks-for-working-with-r-leaflet-and-shiny/
server <- function(input, output) {

  # build data with 2 places
  data=data.frame(x=c(130, 128), y=c(-22,-26), id=c("place1", "place2"))

  # create a reactive value that will store the click position
  data_of_click <- reactiveValues(clickedMarker=NULL,xy=data.frame(x=as.numeric(NA),y=as.numeric(NA)))
  # clicked_xy <- data.frame(x=as.numeric(NA),y=as.numeric(NA),id="none")


  # Leaflet map with 2 markers
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery", group="background 1") %>%
      addTiles(options = providerTileOptions(noWrap = TRUE), group="background 2") %>%
      # setView(lng=131 , lat =-25, zoom=4) %>%
      # addTiles(options = providerTileOptions(noWrap = TRUE)) %>%
      # addCircleMarkers(data=data, ~x , ~y, layerId=~id, popup=~id, radius=8 , color="black",
      #                  fillColor="red", stroke = TRUE, fillOpacity = 0.8) %>%
      addCircleMarkers(data=wells$xy, ~x , ~y, radius=8 , color="black",
                       fillColor="green", stroke = TRUE, fillOpacity = 0.8) %>%
      addCircleMarkers(data=data_of_click$xy, ~x , ~y, radius=8 , color="black",
                       fillColor="blue", stroke = TRUE, fillOpacity = 0.8) %>%
      addLayersControl(baseGroups = c("background 1","background 2"),#overlayGroups = c("Red","Blue") ,
                       options = layersControlOptions(collapsed = FALSE)) %>%
      onRender(
        # https://stackoverflow.com/questions/46132742/coordinates-of-current-mouse-position-on-leaflet-map-with-shiny
        "function(el,x){
                    this.on('mousemove', function(e) {
                        var lat = e.latlng.lat;
                        var lng = e.latlng.lng;
                        var coord = [lat, lng];
                        Shiny.onInputChange('hover_coordinates', coord)
                    });
                    this.on('mouseout', function(e) {
                        Shiny.onInputChange('hover_coordinates', null)
                    })
                }"
      )
  })

  output$mapcoords <- renderText({
    if(is.null(input$hover_coordinates)) {
      "Mouse outside of map"
    } else {
      paste0("Lat: ", input$hover_coordinates[1],
             "\nLng: ", input$hover_coordinates[2])
    }
  })


  wells <- reactiveValues(clickedMarker=NULL,xy=data.frame(x=as.numeric(NA),y=as.numeric(NA)))

  # store the click
  observeEvent(input$map_click,{
    wells$clickedMarker <- input$map_click
    wells$xy <- rbind(wells$xy,data.frame(x=wells$clickedMarker$lng,y=wells$clickedMarker$lat))
  })

  output$out <- renderPrint({
    validate(need(input$map_click, FALSE))

    str(input$map_click)
  })

  # output$newwell <- renderPrint({print(wells)})

  # store the click
  observeEvent(input$map_marker_click,{
    data_of_click$clickedMarker <- input$map_marker_click
    data_of_click$xy <- data.frame(x=data_of_click$clickedMarker$lng,y=data_of_click$clickedMarker$lat)
  })

  output$text <- renderPrint({print(data_of_click$clickedMarker)})

  # Make a barplot or scatterplot depending of the selected point
  output$plot=renderPlot({
    my_place=data_of_click$clickedMarker$id
    if(is.null(my_place)){my_place="place1"}
    if(my_place=="place1"){
      plot(rnorm(1000), col=rgb(0.9,0.4,0.1,0.3), cex=3, pch=20)
    }else{
      barplot(rnorm(10), col=rgb(0.1,0.4,0.9,0.3))
    }
  })
}


ui <- fluidPage(
  br(),
  column(8,leafletOutput("map", height="600px"),
         verbatimTextOutput("mapcoords")),
  # column(4,plotOutput("plot", height="300px")),
  br(),
  h4("text"),
  shiny::verbatimTextOutput("text"),
  # br(),
  # verbatimTextOutput("newwell"),
  # br(),
  br(),
  h4("out"),
  verbatimTextOutput("out")
)

shinyApp(ui = ui, server = server)



#### example from:https://groups.google.com/forum/#!topic/shiny-discuss/qecQQqK-qXw
# library(shiny)
# library(leaflet)
#
# ui <- fluidPage(
#   leafletOutput("map"),
#   br(),
#   verbatimTextOutput("out")
# )
#
# server <- function(input, output, session) {
#   output$map <- renderLeaflet({
#     leaflet() %>% addTiles()
#   })
#
#   output$out <- renderPrint({
#     validate(need(input$map_click, FALSE))
#
#     str(input$map_click)
#   })
# }
#
# shinyApp(ui, server)

