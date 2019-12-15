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
  expect_equal(round(dplyr::select(get_rectangle(boundaries),-x1,-x2,-y1,-y2),4),
               tibble::tibble(m=c(0.9091,-1.1,0.9091,-1.1),
                              b=c(0.4545,22.15,-2.3636,0.05),
                              bID=as.numeric(1:4)))
})



bounds1 <- data.frame(x1=c(0,10,13,1),y1=c(0,10,9,-1),x2=c(10,13,1,0),y2=c(10,9,-1,0))
df1 <- round(tibble::tibble(m=c(0.909090909090909, -1.1, 0.909090909090909, -1.1),
                  b=c(0.454545454545455, 22.15, -2.36363636363636, 0.05),
                  bID=1:4,
                  x1=c(10.7986425339, 10.7986425339, 12.2013574661, -0.2013574661),
                  x2=c(-0.2013574661, 12.2013574661, 1.2013574661, 1.2013574661),
                  y1=c(10.2714932127, 10.2714932127, 8.7285067873, 0.2714932127),
                  y2=c(0.2714932127, 8.7285067873, -1.2714932127, -1.2714932127)),5)
test_that("get_rectangle returns correct bounds for quadrangle",{
  expect_equal(round(get_rectangle(bounds1),5),df1)
})


bounds4 <- data.frame(bID=1:4,x1=c(0,0,10,10),y1=c(0,10,10,0),x2=c(0,10,10,0),y2=c(10,10,0,0))
df4 <- tibble::tibble(m=c(Inf, 0, Inf, 0),
                     b=c(0, 10, 10, 0),
                     bID=1:4,
                     x1=c(0, 0, 10, 0),
                     x2=c(0, 10, 10, 10),
                     y1=c(10, 10, 10, 0),
                     y2=c(0, 10, 0, 0))

# get_rectangle(bounds4) %>% ggp::print_data_frame_for_entry()
test_that("get_rectangle returns correct bounds for square with vertical and horizontal lines",{
  expect_equal(get_rectangle(bounds4),df4)
})


test_that("get_intersection returns proper values for diagonal lines",{
  expect_equal(get_intersection(1,0,-1,2),data.frame(x=1,y=1))
})
test_that("get_intersection returns proper values for vertical lines",{
  expect_equal(get_intersection(Inf,1,0,2),data.frame(x=1,y=2))
})
test_that("get_intersection returns NaN for vertical lines",{
  expect_equal(get_intersection(Inf,1,Inf,2),data.frame(x=NaN,y=NaN))
})

bounds <- data.frame(x1=c(0,10,13,1),y1=c(0,10,9,-1),x2=c(10,13,1,0),y2=c(10,9,-1,0)) %>% dplyr::mutate(bID=row_number())
df <- tibble::tibble(bID=as.integer(c(1, 1, 2, 2, 3, 3, 4, 4)),
                     intersection_bID=as.integer(c(2, 4, 1, 3, 2, 4, 1, 3)),
                     x=c(10, 0, 10, 13, 13, 1, 0, 1),
                     y=c(10, 0, 10, 9, 9, -1, 0, -1))
test_that("get_quad_vertices returns data.frame with proper lines",{
  expect_equal(get_quad_vertices(bounds) %>% dplyr::filter(!is.na(x)),df)
})


bounds <- data.frame(bID=1:4,x1=c(0,0,10,10),y1=c(0,10,10,0),x2=c(0,10,10,0),y2=c(10,10,0,0))
df <- data.frame(bID=as.integer(c(1, 1, 2, 2, 3, 3, 4, 4)),
                 intersection_bID=as.integer(c(2, 4, 1, 3, 2, 4, 1, 3)),
                 x=c(0, 0, 0, 10, 10, 10, 0, 10),
                 y=c(10, 0, 10, 10, 10, 0, 0, 0))
test_that("get_quad_vertices returns data.frame with proper vertices from vertical lines",{
  expect_equal(get_quad_vertices(bounds) %>% dplyr::filter(!is.na(x)),df)
})




bounds <- data.frame(bound_type=c("CH","NF","NF","NF"),m=c(Inf,0,Inf,0),b=c(0,0,100,100)) %>% define_bounds()
loc <- c(150,150)
test_that("get_distance_to_bounds works for single location",{
  expect_equal(round(get_distance_to_bounds(loc,bounds),5),70.71068)
})

bounds <- data.frame(bound_type=c("CH","NF","NF","NF"),m=c(-2,0.5,-2,0.5),b=c(0,0,100,20)) %>% define_bounds()
loc <- data.frame(x=c(-200,0,200,-50),y=c(-200,0,200,50))
test_that("get_distance_to_bounds works for data.frame location",{
  expect_equal(round(get_distance_to_bounds(loc,bounds),4),c(282.8427,0.0000,234.7765,54.0370))
})

# df <- data.frame(bound_x=c(0, 0, 32, -8),
#                  bound_y=c(0, 0, 36, 16),
#                  dist=c(282.8427, 0, 234.7765, 54.037))
# test_that("get_distance_to_bounds works for data.frame location and returns return_locations",{
#   expect_equal(round(get_distance_to_bounds(loc,bounds,TRUE),4),df)
# })

bounds <- data.frame(bound_type=c("CH","NF","NF","NF"),m=c(-2,0.5,-2,0.5),b=c(0,0,100,20)) %>% define_bounds()
loc <- data.frame(x=c(-100,0,100),y=c(-150,0,150))
#' nearest <- get_nearest_point_on_line(loc_rep,m=bounds$m,b=bounds$b)
test_that("get_nearest_point_on_line works for single line and 3 point",{
  expect_equal(get_nearest_point_on_line(loc,bounds[1,]$m,bounds[1,]$b),data.frame(x=c(40,0,-40),y=c(-80,0,80)))
})

loc_rep <- data.frame(x=rep(0,4),y=rep(75,4))
test_that("get_nearest_point_on_line works for single point and 4 lines",{
  expect_equal(get_nearest_point_on_line(loc_rep,m=bounds$m,b=bounds$b),data.frame(x=c(-30,30,10,22),y=c(60,15,80,31)))
})


edges_4 <- data.frame(x1=c(0,0,1,1),y1=c(0,1,1,0),
                      bID=c(1,2,3,4), x2=c(0,1,1,0), y2=c(1,1,0,0))
test_that("get_edges_from_vertices works for 2, 3, and 4 vertices",{
  expect_equal(get_edges_from_vertices(data.frame(x=c(0,0),y=c(0,1),bID=1:2)),edges_4[1,])
  expect_equal(get_edges_from_vertices(data.frame(x=c(0,0,1),y=c(0,1,1),bID=1:3)),edges_4[1:2,])
  expect_equal(get_edges_from_vertices(data.frame(x=c(0,0,1,1),y=c(0,1,1,0),bID=1:4)),edges_4)
})
test_that("get_edges_from_vertices returns no rows for 1 vertice",{
  expect_equal(get_edges_from_vertices(data.frame(x=c(0),y=c(0),bID=1)),edges_4[FALSE,])
})


edges_user <- data.frame(x1=c(-87.38,-86.22,-85.85,-87.18),
                         y1=c(41.44,41.83,41.15,40.85),
                         bID=c(5,6,7,8),
                         x2=c(-86.22,-85.85,-87.18,-87.38),
                         y2=c(41.83,41.15,40.85,41.44))
edges_rect <- df <- data.frame(bID=c(5, 6, 7, 8),
                               x1=c(-87.44, -86.19, -85.88, -87.12),
                               y1=c(41.46, 41.81, 41.17, 40.83),
                               x2=c(-86.19, -85.88, -87.12, -87.44),
                               y2=c(41.81, 41.17, 40.83, 41.46))
test_that("get_utm_rectangle works",{
  expect_equal(get_utm_rectangle(edges_user=edges_user) %>%
                 dplyr::mutate_if(is.numeric,function(x) round(x,2)),edges_rect)
})

