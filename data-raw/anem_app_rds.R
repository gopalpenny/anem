## code to prepare `anem_app_rds` dataset goes here

groundwater_district <- readRDS("inst/anem-app/example_scenarios/groundwater_district.rds")
municipal_contamination <- readRDS("inst/anem-app/example_scenarios/municipal_contamination.rds")

usethis::use_data(groundwater_district,municipal_contamination)
