# see: https://community.rstudio.com/t/sliding-a-point-on-a-plot-rather-than-sliderinput-to-update-plot-in-shiny/16405/8
# objs <- list(
#   list(
#     type = "circle",
#     fillcolor = "gray",
#     line = list(color = "gray"),
#     xsizemode = "point",
#     ysizemode = "point",
#     xanchor = -1, yanchor = 1),
#   list(
#     type = "circle",
#     fillcolor = "gray",
#     line = list(color = "gray"),
#     size=1,
#     xsizemode = "point",
#     ysizemode = "point",
#     xanchor = 0, yanchor = 0)
# )
#
# output$p <- renderPlotly({
#   plot_ly() %>%
#     layout(
#       xaxis = list(range = c(-10, 10)),
#       yaxis = list(range = c(-10, 10)),
#       shapes = objs
#     ) %>%
#     config(editable = TRUE)
#
# })
#
# output$event <- renderPrint({
#   event_data("plotly_relayout")
# })
