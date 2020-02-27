# anem_ui.R

library(leaflet)
library(anem)
library(DT)
library(ggplot2)
library(shinycssloaders)
library(raster)
library(mapview)
library(deSolve)

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
                          id="aquifermode",
                          type="tabs",
                          tabPanel(
                            "Bounds",value="boundaries",fluid=TRUE,
                            HTML("<p style=font-size:45%><br></p>"),
                            #     "Prepare scenario",fluid=TRUE,
                            # conditionalPanel(
                            #   condition="input.usermode == 'aquifer' & input.aquifer_input == 'boundaries'",
                            # h4("Aquifer "),
                            p("Click 4 points to add rectangular aquifer boundaries."), #bound_type can be \"NF\" (no flow) or \"CH\" (constant head)"),
                            fluidRow(
                              # column(6,dataTableOutput("edgetable")),
                              column(5,h5("Bound 1:"),align='right'),
                              column(7,selectInput("b1_type",NULL,choices = c("No flow"="NF","Constant head"="CH","Open boundary"="PB"),selected = "No flow"))
                            ),
                            fluidRow(
                              # column(6,dataTableOutput("edgetable")),
                              column(5,h5("Bound 2:"),align='right'),
                              column(7,selectInput("b2_type",NULL,choices = c("No flow"="NF","Constant head"="CH","Open boundary"="PB"),selected = "No flow"))
                            ),
                            fluidRow(
                              # column(6,dataTableOutput("edgetable")),
                              column(5,h5("Bound 3:"),align='right'),
                              column(7,selectInput("b3_type",NULL,choices = c("No flow"="NF","Constant head"="CH","Open boundary"="PB"),selected = "No flow"))
                            ),
                            fluidRow(
                              # column(6,dataTableOutput("edgetable")),
                              column(5,h5("Bound 4:"),align='right'),
                              column(7,selectInput("b4_type",NULL,choices = c("No flow"="NF","Constant head"="CH","Open boundary"="PB"),selected = "No flow"))
                            )#,verbatimTextOutput("boundtypes")
                          ),
                          tabPanel(
                            "Properties",value="properties",fluid=TRUE,
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
                          ),
                          tabPanel(
                            "Recharge",value="recharge",fluid=TRUE,
                            HTML("<p style=font-size:25%><br></p>"),
                            checkboxInput("enableRecharge","Enable recharge",value=FALSE),
                            p(paste("Click 2 points to set direction of uniform flow.",
                                    "The origin (larger) point maintains the undisturbed aquifer head, h0.")), #bound_type can be \"NF\" (no flow) or \"CH\" (constant head)"),
                            numericInput("rechargeFlow","Unit flow, cumec/meter",0),
                            # verbatimTextOutput("recharge_df"),
                            # conditionalPanel(
                            #   condition = "input.usermode == 'aquifer' & input.aquifer_input == 'properties'",
                            # h5("Aquifer type"),
                            fluidRow(
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
                              column(6,numericInput("pumpingtime_months","t, months",64,0,12*100)),
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
                        shiny::checkboxInput("wellCapture","Well capture zones",value = FALSE),
                        shiny::sliderInput("captureParticles","Number of particles per well",value = 8, min=0, max=32, step = 1),
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
                      ),
                      tabPanel(
                        "File",value="files",
                        hr(),
                        p("Upload a scenario file or download the current scenario. The file should be saved with a .rds extension."),
                        shiny::fileInput("fileUpload","Upload scenario",multiple=FALSE,c(".rds",".Rds",".RDS")),
                        shiny::downloadButton("fileDownload","Download scenario")#,
                        # shiny::verbatimTextOutput("printfile")
                      )
                    )
                  ),
                  # Prepare map
                  column(8,
                         fluidRow(
                           column(6,h3(textOutput("prepmaptitle"))),
                           column(3,align='right',
                                  HTML("<p style=font-size:45%><br><br></p>"),
                                  actionLink("clearMap","Clear map",style='font-size:80%')),
                           column(3,align='right',
                                  HTML("<p style=font-size:45%><br><br></p>"),
                                  actionLink("resetZoomLink","Zoom to objects",style='font-size:80%'))
                         ),
                         leaflet::leafletOutput("prepmap",height=430),
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
                fluidRow(
                  column(12,
                         conditionalPanel(
                           condition="input.usermode == 'wells'",
                           h4("Wells (double click to edit)"),
                           dataTableOutput("welltable"))
                  )
                )#,
                # verbatimTextOutput("aquifer"),
                # verbatimTextOutput("bounds"),
                # verbatimTextOutput("wells")

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
                         dataTableOutput("particletable_output")#,
                         # verbatimTextOutput("capture_endpoint")
                  ),
                  column(8,
                         fluidRow(
                           column(9,h3(textOutput("resultsmaptitle"))),
                           column(3,align='right',
                                  HTML("<p style=font-size:45%><br><br></p>"),
                                  actionLink("resetZoomLink_results","Zoom to objects",style='font-size:80%'))
                         ),
                         leaflet::leafletOutput("resultsmap",height=430) %>% withSpinner(),
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
