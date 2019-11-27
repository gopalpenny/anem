#context("anema_imaging")

library(magrittr)

# get_mirror_point testing
point <- define_wells(tibble::tibble(wID=1,x=0,y=0,Q=0.5,R=5,diam=0.75,path=1,orig_wID=1,w_group="a"))
point_sf <- point %>% dplyr::mutate(X=x,Y=y) %>% sf::st_as_sf(coords=c("X","Y"))
boundary <- tibble::tibble(bID=1,m=-1,b=1,bound_type="NF")


well2 <- define_wells(wID=1,x=2,y=2,Q=0.5,diam=0.75,path=1,orig_wID=1,well_group="a")
boundary2 <- data.frame(bID=1,m=Inf,b=1,bound_type="NF")

well3 <- define_wells(wID=1,x=2,y=2,Q=0.5,diam=0.75,path=1,orig_wID=1,well_group="a")
boundary3 <- data.frame(bID=1,m=0,b=1,bound_type="NF")

test_that("get_mirror_point accurately mirrors the point", {
  expect_equal(get_mirror_point(point, boundary, new_wID=2) %>% dplyr::select(-max_mirror_dist,-well_type),
               tibble::tibble(wID=2,Q=0.5,diam=0.75,x=1,y=1,orig_wID=1,transform="NF",R=5,
                              source_bound=1,path="1 : 1 (1-NF)",w_group="a",well_image="Image (+Q)"))
  expect_equal(get_mirror_point(well2, boundary2, new_wID=2) %>% dplyr::select(x,y),
               tibble::tibble(x=0,y=2))
  expect_equal(get_mirror_point(well3, boundary3, new_wID=2) %>% dplyr::select(x,y),
               tibble::tibble(x=2,y=0))
})

test_that("get_mirror_point reproduces class", {
  expect_equal(class(get_mirror_point(point_sf, boundary, new_wID=2)),
               c("sf","tbl_df","tbl","data.frame"))
})


wells <- define_wells(x=c(0,0.5),y=c(0,0.25),Q=c(0.5,-0.2),R=100,diam=c(0.75,0.8))
bounds <- data.frame(m=c(1,-1),b=c(0.5,1),bound_type=c("CH","NF"),bID=c(1,2))
test_that("mirror_across_bounds generates error if slopes are not equal", {
  expect_error(mirror_across_bounds(wells,bounds,num_levels=1),"mirror_across_bounds can only mirror across parallel boundaries.")
})

well1 <- define_wells(x=50,y=50,Q=20,R=100,diam=1)
well2 <- define_wells(x=25,y=75,Q=20,R=100,diam=1)
wells <- define_wells(dplyr::bind_rows(well1,well2))
bounds_df <- data.frame(bound_type=c("CH","NF","NF","NF"),m=c(Inf,0,Inf,0),b=c(0,0,100,100))
bounds <- define_bounds(bounds_df) %>% dplyr::filter(m==Inf)
df <- tibble::tibble(wID=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                 Q=c(20, 20, -20, 20, -20, 20, -20, -20, -20, -20),
                 R=c(100, 100, 100, 100, 100, 100, 100, 100, 100, 100),
                 diam=c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
                 x=c(50, 25, -50, 150, -25, 175, 250, -150, 225, -175),
                 y=c(50, 75, 50, 50, 75, 75, 50, 50, 75, 75),
                 well_image=c("Actual", "Actual", "Image (-Q)", "Image (+Q)", "Image (-Q)", "Image (+Q)", "Image (-Q)", "Image (-Q)", "Image (-Q)", "Image (-Q)"),
                 orig_wID=as.integer(c(1, 2, 1, 1, 2, 2, 1, 1, 2, 2)),
                 transform=c("none", "none", "CH", "NF", "CH", "NF", "NF", "CH", "NF", "CH"),
                 source_bound=c("none", "none", "1", "3", "1", "3", "3", "1", "3", "1"),
                 path=c("x", "x", "x : 1 (1-CH)", "x : 1 (3-NF)", "x : 2 (1-CH)", "x : 2 (3-NF)", "x : 1 (1-CH) : 3 (3-NF)", "x : 1 (3-NF) : 4 (1-CH)", "x : 2 (1-CH) : 5 (3-NF)", "x : 2 (3-NF) : 6 (1-CH)"),
                 max_mirror_dist=c(0, 0, 50, 50, 25, 75, 150, 150, 125, 175),
                 level=as.integer(c(0, 0, 1, 1, 1, 1, 2, 2, 2, 2)))
test_that("mirror_across_bounds reproduces correct results for simple example", {
  expect_equivalent(mirror_across_bounds(wells,bounds) %>% dplyr::select(-well_type),df)
})

paste(mirror_across_bounds(wells,bounds)$well_image,collapse="\",\"")

# generate_image_wells testing
well1 <- define_wells(x=50,y=50,Q=20,R=100,diam=1)
well2 <- define_wells(x=25,y=75,Q=20,R=100,diam=1)
wells <- define_wells(dplyr::bind_rows(well1,well2))
bounds <- data.frame(bound_type=c("CH","NF","NF","NF"),m=c(Inf,0,Inf,0),b=c(0,0,100,100)) %>% define_bounds()
aquifer <- define_aquifer("unconfined",Ksat=1e-4,bounds=bounds)
df <- tibble::tibble(wID=as.integer(c(1, 2, 3, 4, 5, 6, 11, 12, 13, 14, 15, 16, 17, 18, 19, 21, 22)),
                 Q=c(20, 20, 20, 20, 20, 20, -20, 20, -20, 20, -20, 20, -20, 20, -20, -20, 20),
                 R=c(100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100),
                 diam=c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
                 x=c(50, 25, 50, 50, 25, 25, -50, 150, -25, 175, -50, 150, -50, 150, -25, -25, 175),
                 y=c(50, 75, -50, 150, -75, 125, 50, 50, 75, 75, -50, -50, 150, 150, -75, 125, 125),
                 well_image=c("Actual","Actual","Image (+Q)","Image (+Q)","Image (+Q)","Image (+Q)","Image (-Q)","Image (+Q)","Image (-Q)","Image (+Q)","Image (-Q)","Image (+Q)","Image (-Q)","Image (+Q)","Image (-Q)","Image (-Q)","Image (+Q)"),
                 orig_wID=as.integer(c(1, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 1, 1, 2, 2, 2))) %>%
  dplyr::arrange(x,y) %>% dplyr::select(-wID)
test_that("generate_image_wells returns correct data.frame",{
  expect_equal(generate_image_wells(wells,bounds) %>% dplyr::select(-well_type,-wID) %>% dplyr::arrange(x,y),df)
})

# NOTE: GROUP IMAGING IS CURRENTLY DISABLED IN generate_image_wells().
# test_that("generate_image_wells returns correct data.frame group_imaging=TRUE",{
#   expect_equal(generate_image_wells(wells,bounds,group_imaging=TRUE) %>% dplyr::select(-well_type,-wID) %>% dplyr::arrange(x,y),df)
# })

wells_sf <- wells %>% dplyr::mutate(X=x,Y=y) %>% sf::st_as_sf(coords=c("X","Y"))
test_that("generate_image_wells returns the proper type",{
  expect_equal(class(generate_image_wells(wells_sf,bounds)),
               c("sf","tbl_df","tbl","data.frame"))
})



well1 <- define_wells(x=50,y=50,Q=5,R=100,diam=1)
well2 <- define_wells(x=25,y=75,Q=-2,R=100,diam=1)
wells <- define_wells(dplyr::bind_rows(well1,well2))
bounds <- data.frame(bound_type=c("CH","NF","NF","NF"),m=c(Inf,0,Inf,0),b=c(0,0,100,100)) %>%
  define_bounds()
aquifer <- define_aquifer("unconfined",Ksat=1e-4,bounds=bounds)
image_wells <- generate_image_wells(wells,bounds)
image_wells_image_NA <- image_wells %>%
  dplyr::mutate(Q=dplyr::case_when(grepl("Image",well_image)~as.numeric(NA),TRUE~Q))
test_that("reconstruct_image_pumping accurately reconstructs image pumping for + and - Q, and CH and NF boundaries",{
  expect_equal(reconstruct_image_pumping(image_wells_image_NA),image_wells)
})


test_that("gen_well_image_type works for CH, NF boundary",{
  expect_equal(gen_well_image_type(N=rep(1:5,each=2),well_image=rep("Actual",10),
                                   bound_type1=c(rep("CH",10)),bound_type2=c(rep("NF",10)))$sign,rep(c(-1,-1,1,1,-1),each=2))
})

test_that("gen_well_image_type works for NF, CH boundary",{
  expect_equal(gen_well_image_type(N=rep(1:5,each=2),well_image=rep("Actual",10),
                                   bound_type1=c(rep("NF",10)),bound_type2=c(rep("CH",10)))$sign,rep(c(1,-1,-1,1,1),each=2))
})
test_that("gen_well_image_type works for NF, NF boundary",{
  expect_equal(gen_well_image_type(N=rep(1:5,each=2),well_image=rep("Actual",10),
                                   bound_type1=c(rep("NF",10)),bound_type2=c(rep("NF",10)))$sign,rep(c(1,1,1,1,1),each=2))
})

test_that("gen_well_image_type works for CH, CH boundary",{
  expect_equal(gen_well_image_type(N=rep(1:5,each=2),well_image=rep("Actual",10),
                                   bound_type1=c(rep("CH",10)),bound_type2=c(rep("CH",10)))$sign,rep(c(-1,1,-1,1,-1),each=2))
})


test_that("gen_well_image_type works for CH, CH boundary",{
  expect_equal(gen_well_image_type(N=rep(1:5,each=2),well_image=rep("Image (+Q)",10),
                                   bound_type1=c(rep("CH",10)),bound_type2=c(rep("CH",10)))$sign,rep(c(-1,1,-1,1,-1),each=2))
})

test_that("gen_well_image_type works for NF, CH boundary",{
  expect_equal(gen_well_image_type(N=rep(1:5,each=2),well_image=rep("Image (-Q)",10),
                                   bound_type1=c(rep("NF",10)),bound_type2=c(rep("CH",10)))$sign,rep(c(-1,1,1,-1,-1),each=2))
})
