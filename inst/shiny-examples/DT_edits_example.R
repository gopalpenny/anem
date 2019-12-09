library(shiny)
library(DT)

shinyApp(
  ui = fluidPage(
    DTOutput('x1'),
    tableOutput('edited')
  ),
  server = function(input, output, session) {
    x = iris[1:3,]
    x$Date = Sys.time() + seq_len(nrow(x))
    output$x1 = renderDT(x, selection = 'none', rownames = F, editable = T)

    proxy = dataTableProxy('x1')

    observeEvent(input$x1_cell_edit, {
      info = input$x1_cell_edit
      str(info)
      i = info$row
      j = info$col + 1  # column index offset by 1
      v = info$value
      x[i, j] <<- DT::coerceValue(v, x[i, j])
      replaceData(proxy, x, resetPaging = FALSE, rownames = FALSE)
      str(x)
      output$edited <- renderTable({x})
    })

  }
)

