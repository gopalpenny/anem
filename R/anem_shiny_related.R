#' Run anem shiny app
#'
#' Run anem-app
#'
#' Run anem-app hiny application
#' @export
#' @examples
#' library(anem)
#' \dontrun{
#' anem_app()
#' }
anem_app <- function(shiny_app="anem-app") {
  if (dir.exists(shiny_app)) {
    shiny::runApp(shiny_app)
  } else if (dir.exists(file.path("inst",shiny_app))) {
    shiny::runApp(file.path("inst",shiny_app),display.mode=display.mode)
  } else {
    stop("Could not find anem-app directory in ./ or ./inst/")
  }
}

#' Import app rds
#'
#' Import rds output from anem-app
#' @param path Path to rds file downloaded from web
#' @param params List objected loaded from the rds files
#' @param gen_well_images Boolean value. If \code{TRUE}, output \code{wells} includes well images.
#' @details
#' The rds files downloaded from the web application contain the raw data and map
#' click information needed for the app. This function converts those raw values
#' to UTM coordinates for processing with the R package.
#'
#' Either \code{path} or \code{params} should be supplied to the function. If both
#' are specified, only \code{path} will be used.
#' @return
#' Returns a list containing \code{aquifer}, \code{wells}, and \code{particles} that
#' were input into the web application.
#' @importFrom magrittr %>%
#' @export
#' @examples
#' library(tidyverse)
#' # using built in package data
#' app <- import_app_rds(params=anem_app_scenario)
#'
#' # using RDS file
#' file <- tempfile("anem_app_scenario.rds")
#' saveRDS(anem_app_scenario,file)
#' app <- import_app_rds(file)
#'
#' #' # view the data
#' gridded <- get_gridded_hydrodynamics(app$wells,app$aquifer,c(80,80),c(8,8))
#' ggplot() +
#'   geom_raster(data=gridded$head,aes(x,y,fill=head_m)) +
#'   geom_segment(data=gridded$flow,aes(x,y,xend=x2,yend=y2),
#'                arrow = arrow(ends="last",type="closed",length=unit(1,"mm")),color="black") +
#'   geom_segment(data=app$aquifer$bounds,aes(x1,y1,xend=x2,yend=y2,linetype=bound_type)) +
#'   coord_equal()
import_app_rds <- function(path, params = NULL, gen_well_images = TRUE) {
  # Import rds file
  if (!missing(path)) {
    aa <- readRDS(path)
  } else if (!is.null(params)) {
    aa <- params
  } else {
    stop("Must supply either path or params as input.")
  }

  # Get proj4string in UTM coordinates
  if (nrow(aa$bound_vertices) > 0) {
    utm_zone <- anem::longitude_to_utm_zone(mean(aa$bound_vertices$x))
  } else if(nrow(aa$bound_vertices) > 0) {
    utm_zone <- anem::longitude_to_utm_zone(mean(aa$well_locations$x))
  } else if(nrow(aa$bound_vertices) > 0) {
    utm_zone <- anem::longitude_to_utm_zone(mean(aa$particle_locations$x))
  } else {
    utm_zone <- NULL
  }
  p4s <- anem::utm_zone_to_proj4(utm_zone)

  # Import bound vertices and prepare bounds
  if(nrow(aa$bound_vertices) == 4) {
    bound_types <-c(aa$b1_type,aa$b2_type,aa$b3_type,aa$b4_type)
    bounds <- list()
    bounds$edges_user <- get_edges_from_vertices(aa$bound_vertices)
    bounds$edges_rectangular <-
      use_anem_function("get_utm_rectangle",
                        edges_user=bounds$edges_user) %>%
      dplyr::mutate(bound_type=bound_types) %>%
      dplyr::select(bID,bound_type,dplyr::everything()) %>%
      dplyr::arrange(bID)
    # print("2")
    bounds$bounds_sf <- use_anem_function("bounds_to_sf",bounds$edges_rectangular,crs=4326)

    bounds_utm <- bounds$bounds_sf %>%
      dplyr::select(-dplyr::matches('^[mb]$'),-dplyr::matches("[xy][12]")) %>%
      sf::st_transform(p4s)
    message("Boundaries imported.")
  } else {
    message("No boundaries imported, because there were not 4 aquifer vertices.")
    bounds_utm <- NULL
  }

  # Import recharge
  if (nrow(aa$recharge_vertices) >= 2) {
    rv <- aa$recharge_vertices  %>%
      sf::st_as_sf(coords=c("x","y"),crs=4326) %>%
      sf::st_transform(crs=p4s) %>%
      dplyr::bind_cols(sf::st_coordinates(.) %>% tibble::as_tibble() %>% setNames(c("x","y"))) %>%
      sf::st_set_geometry(NULL)
    recharge_params <- list(recharge_vector=c(rv$x[1],rv$y[1],rv$x[2],rv$y[2]),
                            x0=rv$x[1], y0=rv$y[1],
                            recharge_type = "F",
                            flow = aa$rechargeFlow)
    message("Recharge imported.")
  } else {
    message("No recharge imported.")
    recharge_params <- NULL
  }

  # Define aquifer
  aquifer_utm <- define_aquifer(
    aquifer_type=aa$aquifer_type,
    h0=aa$h0,
    Ksat=aa$Ksat,
    z0=aa$z0,
    n=aa$porosity,
    bounds=bounds_utm,
    recharge=recharge_params
  )
  message("Aquifer properties imported.")

  # Import wells
  if (nrow(aa$well_locations) > 0) {
    wells_utm <- aa$well_locations %>%
      sf::st_as_sf(coords=c("x","y"),crs=4326) %>%
      sf::st_transform(crs=p4s) %>%
      define_wells()
    if (gen_well_images) {
      wells_utm <- wells_utm %>%
        generate_image_wells(aquifer_utm)
    }
    message("Wells imported.")
  } else {
    message("No wells imported.")
    wells_utm <- NULL
  }

  # Import particles
  if (nrow(aa$particle_locations) > 0) {
    particles_utm <- aa$particle_locations %>%
      sf::st_as_sf(coords=c("x","y"),crs=4326) %>%
      sf::st_transform(crs=p4s)
    coords_tbl <- sf::st_coordinates(x) %>% tibble::as_tibble() %>% dplyr::rename(x=X,y=Y)
    particles_utm <- particles_utm %>% dplyr::bind_cols(coords_tbl) %>%
      dplyr::select(pID,x,y,dplyr::everything()) ## FIX -- select only columns that appear in prep scenario tab
    message("Particles imported.")
  } else {
    message("No particles imported.")
    particles_utm <- NULL
  }

  # Return results as a list
  return(list(aquifer=aquifer_utm,wells=wells_utm,particles=particles_utm))
}

