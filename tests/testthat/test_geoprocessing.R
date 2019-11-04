# context("anema_geoprocessing")


test_that("get_utm_zone returns correct zones for three examples", {
  expect_equal(longitude_to_utm_zone(-87), 16)
  expect_equal(longitude_to_utm_zone(6.5), 32)
  expect_equal(longitude_to_utm_zone(77.7), 43)
})



test_that("utm_zone_to_proj4 returns correct proj4string for zone 32", {
  expect_equal(utm_zone_to_proj4(32), "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs")
})
