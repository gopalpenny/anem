context("test_potentials")

test_that("get_radius_of_influence gives proper return for \"cooper-jacob\"",{
  expect_equal(round(get_radius_of_influence(Tr=0.01,t=3600*12,S=1,method="cooper-jacob"),4), 31.1769)
})

test_that("get_radius_of_influence gives proper return for \"aravin-numerov\"",{
  expect_equal(round(get_radius_of_influence(Ksat=0.0001,h=50,t=3600*12,n=0.4,method="aravin-numerov"),4), 32.0312)
})

test_that("get_radius_of_influence gives proper return for \"sichardt\"",{
  expect_equal(round(get_radius_of_influence(Ksat=0.00005,s=10,method="sichardt"),4), 212.132)
})


well <- data.frame(pID=1,x0=0,y0=0,rate=1e-4,diam=0.75,path=1,origin=1,roi=300)
test_that("get_well_effect works in confined case",{
  expect_equal(round(get_well_effect(well,loc=c(50,50),Ksat=0.00001,z0=10,aquifer_type="confined"),4), 0.23)
})

test_that("get_well_effect works in unconfined case",{
  expect_equal(round(get_well_effect(well,loc=c(50,50),Ksat=0.00001,aquifer_type="unconfined"),4), 4.6002)
})


# USING wells
wells <- data.frame(x0=c(0,0.5),y0=c(0,0.25),rate=c(1e-4,-2e-4),diam=c(0.75,0.8),roi=c(300,300))
test_that("get_potential works in confined case",{
  expect_equal(round(get_potential(wells,loc=c(50,50),Ksat=0.00001,z0=10,aquifer_type="confined"),4), -0.2324)
})
test_that("get_potential works in unconfined case",{
  expect_equal(round(get_potential(wells,loc=c(50,50),Ksat=0.00001,aquifer_type="unconfined"),4), -4.6481)
})

### USING wells2. NOTE DISTINCTION WITH wells
wells2 <- data.frame(x0=c(0,0.5),y0=c(0,0.25),rate=c(1e-3,-2e-3),diam=c(0.75,0.8),roi=c(300,300))
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
wells3 <- data.frame(x0=c(0,0.5),y0=c(0,0.25),rate=c(1e-3,-2e-3),diam=c(0.75,0.8),roi=c(300,300))

test_that("get_flowdir works in confined case",{
  expect_equal(round(get_flowdir(wells3,loc=c(5,5),h0=0,Ksat=0.00001,z0=30,aquifer_type="confined"),7), c(-0.0584730,-0.0646688))
})
test_that("get_flowdir works in unconfined case",{
  expect_equal(round(get_flowdir(wells3,loc=c(5,5),h0=30,Ksat=0.00001,aquifer_type="unconfined"),7), c(-0.0629813,-0.0696548))
})
test_that("get_flowdir works for data.frame input",{
  expect_identical(round(get_flowdir(wells3,loc=grid_pts,h0=30,Ksat=0.00001,aquifer_type="unconfined"),4),
               data.frame(dx=c(1.9078,-0.1401,-0.0626,0.0252,-0.063,-0.05,0.0059,-0.0215,-0.0296),
                          dy=c(0.9539,0.0142,0.0031,-0.1244,-0.0697,-0.025,-0.0592,-0.0504,-0.0311)))
})

#' get_flowdir(wells,loc=c(5,5),h0=0,Ksat=0.00001,z0=30,aquifer_type="confined")
