context("test_potentials")

test_that("get_ROI gives proper return for \"cooper-jacob\"",{
  expect_equal(round(get_ROI(Tr=0.01,t=3600*12,S=1,method="cooper-jacob"),4), 31.1769)
})

test_that("get_ROI gives proper return for \"aravin-numerov\"",{
  expect_equal(round(get_ROI(Ksat=0.0001,h=50,t=3600*12,n=0.4,method="aravin-numerov"),4), 32.0312)
})

test_that("get_ROI gives proper return for \"sichardt\"",{
  expect_equal(round(get_ROI(Ksat=0.00005,s=10,method="sichardt"),4), 212.132)
})


well <- data.frame(pID=1,x=0,y=0,Q=1e-4,diam=0.75,path=1,origin=1,roi=300)
test_that("get_well_effect works in confined case",{
  expect_equal(round(get_well_effect(well,loc=c(50,50),Ksat=0.00001,z0=10,aquifer_type="confined"),4), 0.23)
})

test_that("get_well_effect works in unconfined case",{
  expect_equal(round(get_well_effect(well,loc=c(50,50),Ksat=0.00001,aquifer_type="unconfined"),4), 4.6002)
})


# USING wells
wells <- data.frame(x=c(0,0.5),y=c(0,0.25),Q=c(1e-4,-2e-4),diam=c(0.75,0.8),roi=c(300,300))
test_that("get_potential works in confined case",{
  expect_equal(round(get_potential(loc=c(50,50),wells,Ksat=0.00001,z0=10,aquifer_type="confined"),4), -0.2324)
})
test_that("get_potential works in unconfined case",{
  expect_equal(round(get_potential(loc=c(50,50),wells,Ksat=0.00001,aquifer_type="unconfined"),4), -4.6481)
})

### USING wells2. NOTE DISTINCTION WITH wells
wells2 <- data.frame(x=c(0,0.5),y=c(0,0.25),Q=c(1e-3,-2e-3),diam=c(0.75,0.8),roi=c(300,300))


test_that("get_hydraulic_head works in confined case",{
  expect_equal(round(get_hydraulic_head(wells2,loc=c(5,5),h0=0,Ksat=0.00001,z0=30,aquifer_type="confined"),4), -2.0706)
})
test_that("get_hydraulic_head works in unconfined case",{
  expect_equal(round(get_hydraulic_head(wells2,loc=c(5,5),h0=30,Ksat=0.00001,aquifer_type="unconfined"),4), 27.8526)
})

grid_pts <- expand.grid(x=seq(0,10,by=5),y=seq(0,10,by=5))
test_that("get_hydraulic_head works for data.frame input",{
  expect_equal(round(get_hydraulic_head(wells2,loc=grid_pts,h0=30,Ksat=0.00001,aquifer_type="unconfined"),5) %>% as.vector(),
               c(26.69531,27.62361,28.08012,27.69040,27.85255,28.14289,28.11064,28.15529,28.29029))
})


# USING wells3
wells3 <- data.frame(x=c(0,0.5),y=c(0,0.25),Q=c(1e-3,-2e-3),diam=c(0.75,0.8),roi=c(300,300))

test_that("get_flowdir returns value in confined case",{
  expect_equal(round(get_flowdir(wells3,loc=c(5,5),h0=0,Ksat=0.00001,z0=30,aquifer_type="confined"),7), c(-0.0584730,-0.0646688))
})
test_that("get_flowdir returns value in unconfined case",{
  expect_equal(round(get_flowdir(wells3,loc=c(5,5),h0=30,Ksat=0.00001,aquifer_type="unconfined"),7), c(-0.0629813,-0.0696548))
  expect_equal(round(get_flowdir(wells3,loc=c(5,5),h0=30,Ksat=0.00001,aquifer_type="unconfined"),7), c(-0.0629813,-0.0696548))
})
test_that("get_flowdir returns data.frame for data.frame input",{
  expect_identical(round(get_flowdir(wells3,loc=grid_pts,h0=30,Ksat=0.00001,aquifer_type="unconfined"),4),
               data.frame(dx=c(1.9078,-0.1401,-0.0626,0.0252,-0.063,-0.05,0.0059,-0.0215,-0.0296),
                          dy=c(0.9539,0.0142,0.0031,-0.1244,-0.0697,-0.025,-0.0592,-0.0504,-0.0311)))
})


# USING wells4
wells4 <- data.frame(x=c(-10,10),y=c(-10,10),Q=c(1e-3,-1e-3),diam=c(0.1,0.1),roi=c(300,300))
grid_pts4 <- data.frame(x=c(-11,0,11),y=c(-11,0,11))
get_flowdir(wells4,loc=grid_pts4,h0=30,Ksat=0.00001,aquifer_type="unconfined")
test_that("get_flowdir returns flow along diagonal line from injection to pumping well",{
  expect_equal(round(get_flowdir(wells4,loc=grid_pts4,h0=30,Ksat=0.00001,aquifer_type="unconfined"),8) %>%
                     dplyr::mutate(diff=dx-dy,sign=sign(dx)) %>% dplyr::select(diff,sign),
                   data.frame(diff=c(0,0,0),sign=c(-1,1,-1)))
})

# USING wells5
wells5 <- data.frame(x=c(-10,10),y=c(-10,10),Q=c(-1e-3,-1e-3),diam=c(0.1,0.1),roi=c(300,300))
grid_pts5 <- data.frame(x=c(-3,-3,0,3,3),y=c(-3,3,0,-3,3))
ggplot2::ggplot(grid_pts5) +
  ggplot2::geom_point(data=wells5,ggplot2::aes(x,y),color="red") +
  ggplot2::geom_point(ggplot2::aes(x,y)) +
  ggplot2::coord_equal()
test_that("get_flowdir returns no flow along diagonal line between two pumping wells",{
  expect_equal(round(get_flowdir(wells5,loc=grid_pts5,h0=30,Ksat=0.00001,aquifer_type="unconfined"),8) %>%
                 dplyr::mutate(diff=dx-dy,sign=sign(dx),opp=-dx==dy) %>% dplyr::select(diff,sign,opp),
               data.frame(diff=c(0,0.03291868,0,-0.03291868,0),sign=c(-1,1,0,-1,1),opp=c(F,T,T,T,F)))
})
#' get_flowdir(wells,loc=c(5,5),h0=0,Ksat=0.00001,z0=30,aquifer_type="confined")
