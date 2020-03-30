# test_shiny_helpers.R

# file <- list.files("inst/anem-app/example_scenarios",full.names = TRUE)[1] # for debugging
# groundwater_district
file <- tempfile("gwd")
saveRDS(groundwater_district,file)
test_that("import_app_rds imports from file",{
  expect_equal(names(import_app_rds(file)),
               c("aquifer","wells","particles"))
})

app_list <- import_app_rds(params=anem::anem_app_scenario)
test_that("import_app_rds imports from object",{
  expect_equal(names(app_list),
               c("aquifer","wells","particles"))
})

test_that("import_app_rds returns 4 bounds",{
  expect_equal(nrow(app_list$aquifer$bounds),4)
})

test_that("import_app_rds returns 16 wells (including images)",{
  expect_equal(nrow(app_list$wells),16)
})

app_list_no_images <- import_app_rds(params=anem::anem_app_scenario,gen_well_images = FALSE)
test_that("import_app_rds returns 4 wells (without images)",{
  expect_equal(nrow(app_list_no_images$wells),4)
})
