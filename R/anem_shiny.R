# anem_shiny.R

#' Run anem shiny app
#'
#' Run shiny app within package anem
#' @examples
#' run_shiny("anem_shiny")
run_shiny <- function(shiny_app,display.mode="auto") {
  shiny::runApp(file.path("inst/shiny-examples",paste0(shiny_app,".R")),display.mode=display.mode)
}
