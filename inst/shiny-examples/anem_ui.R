# anem_ui.R
library(leaflet)
library(anem)

ui <- fluidPage(
  h4("Groundwater behavior"),
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
                       c("Define aquifer properties" = "aquifer",
                         "Set aquifer bounds" = "bounds",
                         "Add or modify wells" = "wells")),
          conditionalPanel(
            condition = "input.usermode == 'aquifer'",
            h5("Aquifer type"),
            radioButtons("aquifer_type", NULL,
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
            h5("Instructions"),
            textOutput("prepinstr")
          )
        ),
        # Prepare map
        column(8,
               leafletOutput("map",height=420)
        ),
      ),
      fluidRow(
        hr(),
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
  bound_vertices <- reactiveValues(
    clickedMarker=NULL,
    vert=data.frame(x=as.numeric(NA),y=as.numeric(NA)),id=as.integer(NA))

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

  output$map <- renderLeaflet({
    leaflet() %>%
      # addProviderTiles("Esri.WorldImagery", group="background 1") %>%
      addTiles(options = providerTileOptions(noWrap = TRUE), group="background 2") %>%
      setView(lng=-120 , lat =35, zoom=7)

  })

  output$aquifer <- renderPrint({print(aquifer())})
}

shinyApp(ui, server)
