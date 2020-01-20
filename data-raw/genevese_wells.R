## code to prepare `genevese_wells` dataset goes here


set.seed(100)
genevese_wells <- sf::read_sf("/Users/gopal/Projects/CNH/genevois/spatial/shp/stylized_aquifer/genevois_wells_stylized.shp") %>%
  dplyr::mutate(diam=round(runif(dplyr::n(),0.5,2),1)) %>%
  dplyr::select(diam,well_group=CONCESSION) %>%
  dplyr::group_by(well_group) %>%
  dplyr::sample_frac(0.55) %>%
  dplyr::group_by()
# ggplot() + geom_sf(data=genevese_wells,aes(color=well_group))

usethis::use_data(genevese_wells)
