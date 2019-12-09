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
  shiny::plotOutput("domain_plot"),

  h3("Wells"),
  DT::dataTableOutput("x1")#,
  # shiny::tableOutput("edited")

)

# Define server logic to plot various variables against mpg ----
server <- function(input, output, session) {
  wells <- wells_ORIG[1:8,]
  bounds <- data.frame(bound_type=c("CH","NF","NF","NF"),m=c(Inf,0,Inf,0),b=c(0,0,1000,1000)) %>% define_bounds()
  aquifer <- define_aquifer("unconfined",Ksat=1e-4,h0=100,bounds=bounds)

  p_wells <- reactive({ggplot2::ggplot() +
      ggplot2::geom_segment(data=bounds,ggplot2::aes(x1,y1,xend=x2,yend=y2,color=bound_type)) +
      ggplot2::geom_point(data=wells,ggplot2::aes(x,y,fill=Group),shape=21,size=3) +
      # ggplot2::scale_shape_manual() +
      ggplot2::coord_equal()})
  output$domain_plot <- shiny::renderPlot({p_wells()})

  proxy <- DT::dataTableProxy('x1')

  observeEvent(input$x1_cell_edit, {
    info = input$x1_cell_edit
    str(info)
    i = info$row
    j = info$col
    v = info$value
    wells[i, j] <<- DT::coerceValue(v, wells[i, j])
    replaceData(proxy, wells, resetPaging = FALSE, rownames = T)
    output$edited <- renderTable({wells})
    p_wells <- reactive({ggplot2::ggplot() +
        ggplot2::geom_segment(data=bounds,ggplot2::aes(x1,y1,xend=x2,yend=y2,color=bound_type)) +
        ggplot2::geom_point(data=wells,ggplot2::aes(x,y,fill=Group),shape=21,size=3) +
        # ggplot2::scale_shape_manual() +
        ggplot2::coord_equal()})
    output$domain_plot <- shiny::renderPlot({p_wells()})
  })



  # https://shiny.rstudio.com/gallery/basic-datatable.html
  # output$wells <-
  output$x1 <- DT::renderDT(wells, selection = 'none',rownames = T,editable=TRUE)

  # output$edited <- shiny::renderTable({wells_output})
  # output$edited <- shiny::renderPrint(cell_changes())

}

# Create Shiny app ----
shiny::shinyApp(ui = ui, server = server)
