# old_anem_geoprocessing.R


#' Bounds to SF 2
#'
#' Get sf object from boundaries
#' @param bounds A data.frame containing x1, y1, x2, y2, bID
#' @param crs crs object for sf package
#' @return
#' returns object
#' @keywords internal
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#' bounds <- define_bounds(data.frame(m=c(1,-1,1,-1),b=c(0,2,2,4),bound_type=c("CH","NF","NF","NF")))
#' bounds_sf <- bounds_to_sf(bounds, crs=4326)
#' bounds_sf <- use_anem_function("bounds_to_sf",bounds=bounds,crs=4326)
#' }
bounds_to_sf2 <- function(bounds, crs) {
  bounds_linestring <- bounds %>% dplyr::select(-dplyr::matches("^[mb]$")) %>%
    tidyr::gather(coord,val,dplyr::matches("[xy][12]")) %>%
    tidyr::separate(coord,c("axis","point"),1) %>%
    tidyr::spread(axis,val) %>%
    sf::st_as_sf(coords=c("x","y"),crs=crs) %>%
    dplyr::group_by(bID) %>%
    dplyr::summarize() %>%
    sf::st_cast("LINESTRING")
  bounds_sf <-  bounds %>%
    dplyr::left_join(bounds_linestring ,by=c("bID")) %>% sf::st_as_sf(crs=crs)
  return(bounds_sf)
}
