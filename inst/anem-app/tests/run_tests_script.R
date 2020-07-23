library(shinytest)
# app_dir <- "."
app_dir <- "inst/anem-app/tests/"

# test_gw_district -- note: PNG files may not match exactly
testApp(app_dir, "test_gw_district")

# test_anem_app_basics -- note: PNG files may not match exactly
testApp(app_dir, "test_anem_app_prep")

# test_anem_app_well_capture -- note: PNG files may not match exactly
testApp(app_dir, "test_anem_app_well_capture")

# test_anem_app_well_capture -- note: PNG files may not match exactly
testApp(app_dir, "test_particles")

# test_gwdistricts_well_capture -- note: PNG files may not match exactly
testApp(app_dir, "test_gwdistricts_well_capture")
