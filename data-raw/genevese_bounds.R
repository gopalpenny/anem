## code to prepare `genevese_bounds` dataset goes here

genevese_bounds <- sf::read_sf("/Users/gopal/Projects/CNH/genevois/spatial/shp/stylized_aquifer/genevois_bounds_stylized.shp")

usethis::use_data(genevese_bounds)
