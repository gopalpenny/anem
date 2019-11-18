# context("anema_geoprocessing")


test_that("get_utm_zone returns correct zones for three examples", {
  expect_equal(longitude_to_utm_zone(-87), 16)
  expect_equal(longitude_to_utm_zone(6.5), 32)
  expect_equal(longitude_to_utm_zone(77.7), 43)
})



test_that("utm_zone_to_proj4 returns correct proj4string for zone 32", {
  expect_equal(utm_zone_to_proj4(32), "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs")
})


boundaries <- data.frame(x1=c(0,10,13,1),y1=c(0,10,9,-1),x2=c(10,13,1,0),y2=c(10,9,-1,0)) %>% dplyr::mutate(bID=row_number())
test_that("get_rectangle returns data.frame from simple quadrangle",{
  expect_equal(round(get_rectangle(boundaries),4),
               tibble::tibble(m=c(0.9091,-1.1,0.9091,-1.1),
                              b=c(0.4545,22.15,-2.3636,0.05),
                              bID=as.numeric(1:4),
                              bGroup=c(2,1,2,1)))
})
