# context("anema_geoprocessing")
# library(dplyr)

test_that("get_utm_zone returns correct zones for three examples", {
  expect_equal(longitude_to_utm_zone(-87), 16)
  expect_equal(longitude_to_utm_zone(6.5), 32)
  expect_equal(longitude_to_utm_zone(77.7), 43)
})



test_that("utm_zone_to_proj4 returns correct proj4string for zone 32", {
  expect_equal(utm_zone_to_proj4(32), "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs")
})


boundaries <- data.frame(x1=c(0,10,13,1),y1=c(0,10,9,-1),x2=c(10,13,1,0),y2=c(10,9,-1,0)) %>%
  dplyr::mutate(bID=dplyr::row_number())
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
                  y1=c(10.2714932127, 10.2714932127, 8.7285067873, 0.2714932127),
                  x2=c(-0.2013574661, 12.2013574661, 1.2013574661, 1.2013574661),
                  y2=c(0.2714932127, 8.7285067873, -1.2714932127, -1.2714932127)),5)
test_that("get_rectangle returns correct bounds for quadrangle",{
  expect_equal(round(get_rectangle(bounds1),5),df1)
})

bounds4 <- data.frame(bID=1:4,x1=c(0,0,10,10),y1=c(0,10,10,0),x2=c(0,10,10,0),y2=c(10,10,0,0))
df4 <- tibble::tibble(m=c(Inf, 0, Inf, 0),
                     b=c(0, 10, 10, 0),
                     bID=1:4,
                     x1=c(0, 0, 10, 0),
                     y1=c(10, 10, 10, 0),
                     x2=c(0, 10, 10, 10),
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

bounds <- data.frame(x1=c(0,10,13,1),y1=c(0,10,9,-1),x2=c(10,13,1,0),y2=c(10,9,-1,0)) %>% dplyr::mutate(bID=dplyr::row_number())
df <- tibble::tibble(bID=as.integer(c(1, 1, 2, 2, 3, 3, 4, 4)),
                     intersection_bID=as.integer(c(2, 4, 1, 3, 2, 4, 1, 3)),
                     x=c(10, 0, 10, 13, 13, 1, 0, 1),
                     y=c(10, 0, 10, 9, 9, -1, 0, -1))
test_that("get_quad_vertices returns data.frame with proper lines",{
  expect_equal(get_quad_vertices(bounds) %>% dplyr::filter(!is.na(x)),df)
})


bounds <- data.frame(bID=1:4,x1=c(0,0,10,10),y1=c(0,10,10,0),x2=c(0,10,10,0),y2=c(10,10,0,0))
df <- tibble::tibble(bID=as.integer(c(1, 1, 2, 2, 3, 3, 4, 4)),
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
edges_rect <- tibble::tibble(bID=c(5, 6, 7, 8),
                               x1=c(-87.44, -86.19, -85.88, -87.12),
                               y1=c(41.46, 41.81, 41.17, 40.83),
                               x2=c(-86.19, -85.88, -87.12, -87.44),
                               y2=c(41.81, 41.17, 40.83, 41.46))
test_that("get_utm_rectangle works",{
  expect_equal(get_utm_rectangle(edges_user=edges_user) %>%
                 dplyr::mutate_if(is.numeric,function(x) round(x,2)),edges_rect)
})

df <- tidyr::crossing(x=seq(0,5,length.out=100),y=seq(0,5,length.out=100)) %>% dplyr::mutate(z=sqrt(x^2+y^2))
test_that("get_contour_lines doesn't fail",{
  expect_silent(get_contourlines(df,levels=seq(1,120,by=10), type="sf"))
})


df <- tidyr::crossing(x=0:10,y=0:10) %>% dplyr::mutate(z=x^2)
test_that("get_contour_lines doesn't fail",{
  expect_equal(unique(get_contourlines(df,nlevels=5)$x) %>% length(), 5)
})



test_that("get_perpendicular_line works for slope = 0, Inf, or real number",{
  expect_equal(get_perpendicular_line(Inf,2,3),list(m=0,b=3))
  expect_equal(get_perpendicular_line(0,2,3),list(m=Inf,b=2))
  expect_equal(get_perpendicular_line(1/2,2,2),list(m=-2,b=6))
  expect_equal(get_perpendicular_line(-2,2,2),list(m=1/2,b=1))
})

df_circle_path <- data.frame(x=c(3, 2.29813332935693, 0.520944533000791, -1.5, -2.81907786235772, -2.81907786235773, -1.5, 0.52094453300079, 2.29813332935693, 3), y=c(0, 1.92836282905962, 2.95442325903662, 2.59807621135332, 1.02606042997701, -1.02606042997701, -2.59807621135332, -2.95442325903662, -1.92836282905962, -7.34788079488412e-16))
test_that("gen_circleFun works for 1 circle",{
  expect_equal(TRUE,
               all.equal(gen_circleFun(list(x=0,y=0,r=3),npoints = 10),df_circle_path,tolerance=1e-5))
})

circle_obj <- data.frame(x=0,y=0,r=1, id = 2, foo="hello", bar = "bye")
circle_out1 <- gen_circleFun(circle_obj, npoints = 4)
circle_check1 <- data.frame(x=c(1, -0.5, -0.5, 1), y=c(0, 0.866025403784439, -0.866025403784438, -2.44929359829471e-16), id=c(2, 2, 2, 2))
circle_out2 <- gen_circleFun(circle_obj, npoints = 4, include_data=TRUE)
circle_check2 <- data.frame(x=c(1, -0.5, -0.5, 1), y=c(0, 0.866025403784439, -0.866025403784438, -2.44929359829471e-16), id=c(2, 2, 2, 2), r=c(1, 1, 1, 1), foo=c("hello", "hello", "hello", "hello"), bar=c("bye", "bye", "bye", "bye"))
test_that("gen_circleFun works for additional columns",{
  expect_equal(circle_out1, circle_check1)
  expect_equal(circle_out2, circle_check2)
})


circle_df <- data.frame(x=1:3,y=1:3,r=c(0.5,1,1.5), foo="hello")
circles_w_id <- circle_df
circles_w_id$id <- 2
circles_out <- gen_circles(circle_df, npoints = 100)
circles_w_data_out <- gen_circles(circle_df, npoints = 2, include_data = TRUE)
circles_w_data_check <- data.frame(x=c(1.5, 1.5, 3, 3, 4.5, 4.5), y=c(1, 1, 2, 2, 3, 3), id=c(1, 1, 2, 2, 3, 3), r=c(0.5, 0.5, 1, 1, 1.5, 1.5), foo=c("hello", "hello", "hello", "hello", "hello", "hello"))
test_that("gen_circles works for 3 circles and warning for id column",{
  expect_equal(c(300,3),dim(circles_out))
  expect_equal(circles_w_data_out,circles_w_data_check)
  expect_warning(gen_circles(circles_w_id),"df contains id as a column")
})

bounds_a <- define_bounds(data.frame(m=c(1,-1,1,-1),b=c(0,2,2,4),bound_type=c("CH","NF","NF","NF")))
test_that("bounds_to_sf creates an sf object",{
  expect_equal(any(grepl("sf",class(use_anem_function("bounds_to_sf",bounds=bounds_a,crs=4326)))),
               TRUE)
  expect_equal(any(grepl("sf",class(bounds_to_sf(bounds_a, crs=4326)))),
               TRUE)
})

gw_district <- import_app_rds(params=anem::groundwater_district)
bounds_sf <- gw_district$aquifer$bounds
bounds_polygon <- bounds_sf_to_polygon(bounds_sf)
test_that("bounds_sf_to_polygon works for groundwater_district",{
  expect_equal(nrow(bounds_polygon),1)
  expect_equal(any(grepl("sf",class(bounds_polygon))),TRUE)
})
