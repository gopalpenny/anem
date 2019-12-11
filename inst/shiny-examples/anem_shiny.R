library(shiny)
library(anem)
library(DT)

# how to include Shiny in R package: https://deanattali.com/2015/04/21/r-package-shiny-app/
set.seed(30)
wells_ORIG <-  data.frame(x=round(runif(8,0,1000),3),y=round(runif(8,0,1000),3),diam=1) %>%
  dplyr::mutate(R=1000,  # t = 1 year
                Group=as.character(factor(y>500,levels=c(F,T),labels=c("A","B")))) %>%
  dplyr::group_by(Group) %>%
  dplyr::mutate(Weight=1,Q=round(1/dplyr::n(),2),) %>% dplyr::group_by() %>%
  define_wells() %>% as.data.frame() #%>% generate_image_wells(aquifer_unconfined)


# Define UI for miles per gallon app ----
ui <- shiny::fluidPage(

  # App title ----
  shiny::headerPanel("anem imaging example"),

  # Main panel for displaying outputs ----
  h3("Aquifer plot"),

  shiny::fluidRow(
    column(width=6,
           shiny::plotOutput("plot1", #height = 400,
                             # Equivalent to: click = clickOpts(id = "plot_click")
                             click = "plot1_click",
                             brush = brushOpts(
                               id = "plot1_brush")
           )
    )
  ),

  h3("Wells"),
  DT::dataTableOutput("wells_table"),

  fluidRow(
    column(width = 6,
           h4("Points near click"),
           verbatimTextOutput("click_info")
    ),
    column(width = 6,
           h4("Brushed points"),
           verbatimTextOutput("brush_info")
    )
  )

)

# Define server logic to plot various variables against mpg ----
server <- function(input, output, session) {
  wells <- wells_ORIG[1:8,]
  bounds <- data.frame(bound_type=c("CH","NF","NF","NF"),m=c(Inf,0,Inf,0),b=c(0,0,1000,1000)) %>% define_bounds()
  aquifer <- define_aquifer("unconfined",Ksat=1e-4,h0=100,bounds=bounds)

  p_wells <- reactive({ggplot2::ggplot() +
      # ggplot2::geom_segment(data=bounds,ggplot2::aes(x1,y1,xend=x2,yend=y2,color=bound_type)) +
      ggplot2::geom_point(data=wells,ggplot2::aes(x,y,fill=Group),shape=21,size=3) +
      # ggplot2::scale_shape_manual() +
      ggplot2::coord_equal()})
  output$plot1 <- shiny::renderPlot({p_wells()})

  proxy <- DT::dataTableProxy('wells_table')

  observeEvent(input$wells_table_cell_edit, {
    info = input$wells_table_cell_edit
    str(info)
    i = info$row
    j = info$col
    v = info$value
    wells[i, j] <<- DT::coerceValue(v, wells[i, j])
    replaceData(proxy, wells %in% c(brushedPoints(wells, input$plot1_brush)$wID),
                resetPaging = FALSE, rownames = T)
    # output$edited <- renderTable({wells})
    p_wells <- reactive({ggplot2::ggplot() +
        # ggplot2::geom_segment(data=bounds,ggplot2::aes(x1,y1,xend=x2,yend=y2,color=bound_type)) +
        ggplot2::geom_point(data=wells,ggplot2::aes(x,y,fill=Group),shape=21,size=3) +
        # ggplot2::scale_shape_manual() +
        ggplot2::coord_equal()})
    output$plot1 <- shiny::renderPlot({p_wells()})
  })

  # https://shiny.rstudio.com/gallery/basic-datatable.html
  # output$wells <-
  output$wells_table <- DT::renderDT(wells, selection = 'none',rownames = T,editable=TRUE)

  output$click_info <- renderPrint({
    # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
    # were a base graphics plot, we'd need those.
    nearPoints(wells, input$plot1_click, addDist = TRUE)$wID
  })

  output$brush_info <- renderPrint({
    brushedPoints(wells, input$plot1_brush)$wID
  })

}

# Create Shiny app ----
shiny::shinyApp(ui = ui, server = server)
