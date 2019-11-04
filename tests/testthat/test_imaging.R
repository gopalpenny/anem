#context("anema_imaging")

library(magrittr)

# get_mirror_point testing
point <- tibble::tibble(pID=1,x0=0,y0=0,rate=0.5,diam=0.75,path=1,origin=1)
point_sf <- point %>% dplyr::mutate(X=x0,Y=y0) %>% sf::st_as_sf(coords=c("X","Y"))
boundary <- tibble::tibble(bID=1,m=-1,b=1,bound_type="NF")

test_that("get_mirror_point accurately mirrors the point", {
  expect_equal(get_mirror_point(point, boundary, new_pID=2),
               tibble::tibble(pID=2,rate=0.5,diam=0.75,x0=1,y0=1,origin=1,transform="NF",
                              source_bound=1,path="1 : 1 (1-NF)"))
})

test_that("get_mirror_point reproduces class", {
  expect_equal(class(get_mirror_point(point_sf, boundary, new_pID=2)),
               c("sf","tbl_df","tbl","data.frame"))
})


# generate_image_wells testing
wells <- tibble::tibble(x0=c(0,0.5),y0=c(0,0.25),rate=c(0.5,-0.2),diam=c(0.75,0.8))
wells_sf <- wells %>% dplyr::mutate(X=x0,Y=y0) %>% sf::st_as_sf(coords=c("X","Y"))
bounds <- tibble::tibble(m=c(1,-1),b=c(0.5,1),bound_type=c("CH","NF"),bID=c(1,2))

test_that("generate_image_wells returns the proper data.frame shape",{
  expect_equal(dim(generate_image_wells(wells,bounds,num_levels=1)),c(6,10))
  expect_equal(dim(generate_image_wells(wells,bounds,num_levels=2)),c(10,10))
})

test_that("generate_image_wells returns the proper type",{
  expect_equal(class(generate_image_wells(wells_sf,bounds,num_levels=1)),
               c("sf","tbl_df","tbl","data.frame"))
})
