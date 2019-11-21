#context("anema_imaging")

library(magrittr)

# get_mirror_point testing
point <- tibble::tibble(wID=1,x=0,y=0,Q=0.5,R=5,diam=0.75,path=1,origin=1,w_group="a")
point_sf <- point %>% dplyr::mutate(X=x,Y=y) %>% sf::st_as_sf(coords=c("X","Y"))
boundary <- tibble::tibble(bID=1,m=-1,b=1,bound_type="NF")


well2 <- define_wells(wID=1,x=2,y=2,Q=0.5,diam=0.75,path=1,origin=1,well_group="a")
boundary2 <- data.frame(bID=1,m=Inf,b=1,bound_type="NF")

well3 <- define_wells(wID=1,x=2,y=2,Q=0.5,diam=0.75,path=1,origin=1,well_group="a")
boundary3 <- data.frame(bID=1,m=0,b=1,bound_type="NF")

test_that("get_mirror_point accurately mirrors the point", {
  expect_equal(get_mirror_point(point, boundary, new_wID=2) %>% dplyr::select(-max_mirror_dist),
               tibble::tibble(wID=2,Q=0.5,diam=0.75,x=1,y=1,origin=1,transform="NF",R=5,
                              source_bound=1,path="1 : 1 (1-NF)",w_group="a"))
  expect_equal(get_mirror_point(well2, boundary2, new_wID=2) %>% dplyr::select(x,y),
               tibble::tibble(x=0,y=2))
  expect_equal(get_mirror_point(well3, boundary3, new_wID=2) %>% dplyr::select(x,y),
               tibble::tibble(x=2,y=0))
})

test_that("get_mirror_point reproduces class", {
  expect_equal(class(get_mirror_point(point_sf, boundary, new_wID=2)),
               c("sf","tbl_df","tbl","data.frame"))
})


# generate_image_wells testing
wells <- define_wells(x=c(5,0.5),y=c(2.5,0.25),Q=c(0.5,-0.2),diam=c(0.75,0.8),w_group=c("a","b"))
wells_sf <- wells %>% dplyr::mutate(X=x,Y=y) %>% sf::st_as_sf(coords=c("X","Y"))
bounds <- tibble::tibble(bound_type=c("CH","NF","NF","NF"),
                         m=c(0.8,-1.25,0.8,-1.25),b=c(0.3,10,-2.5,0.1),
                         bID=as.numeric(1:4),bGroup=c(2,1,2,1))

test_that("generate_image_wells returns the proper data.frame shape",{
  expect_equal(dim(generate_image_wells(wells,bounds,num_levels=1)),c(18,14))
  expect_equal(dim(generate_image_wells(wells,bounds,num_levels=2)),c(50,14))
})

test_that("generate_image_wells returns the proper type",{
  expect_equal(class(generate_image_wells(wells_sf,bounds,num_levels=1)),
               c("sf","tbl_df","tbl","data.frame"))
})
