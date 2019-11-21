## code to prepare `genevese_wells` dataset goes here


genevese_wells <- sf::read_sf("/Users/gopal/Projects/CNH/genevois/spatial/shp/stylized_aquifer/genevois_wells_stylized.shp") %>%
  mutate(diam=RADUIS*2) %>% select(diam,well_group=CONCESSION)

usethis::use_data(genevese_wells)
