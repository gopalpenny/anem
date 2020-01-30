# context("test_potentials")

test_that("get_ROI gives proper return for \"cooper-jacob\"",{
  expect_equal(round(get_ROI(Tr=0.01,t=3600*12,S=1,method="cooper-jacob"),4), 31.1769)
})

test_that("get_ROI gives proper return for \"aravin-numerov\"",{
  expect_equal(round(get_ROI(Ksat=0.0001,h=50,t=3600*12,n=0.4,method="aravin-numerov"),4), 32.0312)
})

test_that("get_ROI gives proper return for \"sichardt\"",{
  expect_equal(round(get_ROI(Ksat=0.00005,s=10,method="sichardt"),4), 212.132)
})

# USING wells
wells <- define_wells(x=c(0,0.5),y=c(0,0.25),Q=c(1e-4,-2e-4),diam=c(0.75,0.8),R=c(300,300))
aquifer_confined <- define_aquifer(aquifer_type="confined",Ksat=0.00001,h0=0,z0=10)
test_that("get_potential works in confined case",{
  expect_equal(round(get_potential_differential(loc=c(50,50),wells,aquifer_confined),4), -0.2324)
})

aquifer_unconfined <- define_aquifer(aquifer_type="unconfined",Ksat=0.00001)
test_that("get_potential works in unconfined case",{
  expect_equal(round(get_potential_differential(loc=c(50,50),wells,aquifer_unconfined),4), -4.6481)
})

test_that("get_potential_differentiall returns 0 when wells is missing from input",{
  expect_equal(get_potential_differential(c(1,1)),0)
  expect_equal(get_potential_differential(data.frame(x=1:10,y=11:20)),rep(0,10))
})

### USING wells2. NOTE DISTINCTION WITH wells
wells2 <- define_wells(x=c(0,0.5),y=c(0,0.25),Q=c(1e-3,-2e-3),diam=c(0.75,0.8),R=c(300,300))
aquifer_confined2 <- define_aquifer(aquifer_type="confined",Ksat=0.00001,h0=0,z0=30)


test_that("get_hydraulic_head works in confined case",{
  expect_equal(round(get_hydraulic_head(loc=c(5,5),wells2,aquifer_confined2),4), -2.0706)
})

aquifer_unconfined2 <- define_aquifer(aquifer_type="unconfined",Ksat=0.00001,h0=30)


test_that("get_hydraulic_head works in unconfined case",{
  expect_equal(round(get_hydraulic_head(c(5,5),wells2,aquifer_unconfined2),4), 27.8526)
})

grid_pts <- expand.grid(x=seq(0,10,by=5),y=seq(0,10,by=5))
test_that("get_hydraulic_head works for data.frame input",{
  expect_equal(round(get_hydraulic_head(grid_pts,wells2,aquifer_unconfined2),5) %>% as.vector(),
               c(26.69531,27.62361,28.08012,27.69040,27.85255,28.14289,28.11064,28.15529,28.29029))
})


# USING wells3
wells3 <- data.frame(x=c(0,0.5),y=c(0,0.25),Q=c(1e-3,-2e-3),diam=c(0.75,0.8),R=c(300,300))
aquifer3 <- define_aquifer(h0=0,Ksat=0.00001,z0=30,aquifer_type="confined")
test_that("get_flowdir returns value in confined case",{
  expect_equal(round(get_flowdir(loc=c(5,5),wells3,aquifer3),7), c(-0.0584730,-0.0646688))
})

aquifer3 <- define_aquifer(h0=30,Ksat=0.00001,aquifer_type="unconfined")
test_that("get_flowdir returns value in unconfined case",{
  expect_equal(round(get_flowdir(loc=c(5,5),wells3,aquifer3),7), c(-0.0629813,-0.0696548))
  expect_equal(round(get_flowdir(loc=c(5,5),wells3,aquifer3),7), c(-0.0629813,-0.0696548))
})
test_that("get_flowdir returns data.frame for data.frame input",{
  expect_identical(round(get_flowdir(wells3,loc=grid_pts,aquifer3),4),
               data.frame(dx=c(1.9078,-0.1401,-0.0626,0.0252,-0.063,-0.05,0.0059,-0.0215,-0.0296),
                          dy=c(0.9539,0.0142,0.0031,-0.1244,-0.0697,-0.025,-0.0592,-0.0504,-0.0311)))
})

# USING wells4
wells4 <- data.frame(x=c(-10,10),y=c(-10,10),Q=c(1e-3,-1e-3),diam=c(0.1,0.1),R=c(300,300))
grid_pts4 <- data.frame(x=c(-11,0,11),y=c(-11,0,11))
get_flowdir(loc=grid_pts4,wells4,aquifer3)
test_that("get_flowdir returns flow along diagonal line from injection to pumping well",{
  expect_equal(round(get_flowdir(wells4,loc=grid_pts4,aquifer3),8) %>%
                     dplyr::mutate(diff=dx-dy,sign=sign(dx)) %>% dplyr::select(diff,sign),
                   data.frame(diff=c(0,0,0),sign=c(-1,1,-1)))
})

# USING wells5
wells5 <- data.frame(x=c(-10,10),y=c(-10,10),Q=c(-1e-3,-1e-3),diam=c(0.1,0.1),R=c(300,300))
grid_pts5 <- data.frame(x=c(-3,-3,0,3,3),y=c(-3,3,0,-3,3))
ggplot2::ggplot(grid_pts5) +
  ggplot2::geom_point(data=wells5,ggplot2::aes(x,y),color="red") +
  ggplot2::geom_point(ggplot2::aes(x,y)) +
  ggplot2::coord_equal()
test_that("get_flowdir returns no flow along diagonal line between two pumping wells",{
  expect_equal(round(get_flowdir(wells5,loc=grid_pts5,aquifer3),8) %>%
                 dplyr::mutate(diff=dx-dy,sign=sign(dx),opp=-dx==dy) %>% dplyr::select(diff,sign,opp),
               data.frame(diff=c(0,0.03291868,0,-0.03291868,0),sign=c(-1,1,0,-1,1),opp=c(F,T,T,T,F)))
})

### TEST RECHARGE
no_wells <- define_wells(Q=0,x=10,y=10,R=10,diam=1)
recharge_params <- list(recharge_type="F",recharge_vector=c(10,10,11,11),flow=sqrt(2),x0=11,y0=11)
con_aquifer <- define_aquifer("confined",1,h0=0,z0=1,recharge=recharge_params)
rech_loc <- expand.grid(x=9:11,y=9:11)
head <- c(4,3,2,3,2,1,2,1,0) #get_hydraulic_head(rech_loc,no_wells,con_aquifer) %>% paste(collapse=",")
df <- data.frame(dx=rep(1,9),
                 dy=rep(1,9))
test_that("get_hydraulic_head works for \"F\" recharge",{
  expect_equal(round(get_hydraulic_head(rech_loc,no_wells,con_aquifer),3),head)
})
test_that("get_flowdir works for \"F\" recharge",{
  expect_equal(round(get_flowdir(rech_loc,no_wells,con_aquifer),3),df)
})

recharge_params <- list(recharge_type="D",recharge_vector=c(10,10,11,11),flow_main=sqrt(2),flow_opp=sqrt(2),x0=11,y0=11)
con_aquifer <- define_aquifer("confined",1,h0=0,z0=1,recharge=recharge_params)
head <- c(0,1,2,1,2,1,2,1,0) #get_hydraulic_head(rech_loc,no_wells,con_aquifer) %>% round(3) %>% paste(collapse=",")
df <- data.frame(dx=c(-1,-1,0,-1,0,1,0,1,1),
                 dy=c(-1,-1,0,-1,0,1,0,1,1))
test_that("get_hydraulic_head works for \"D\" recharge, confined",{
  expect_equal(round(get_hydraulic_head(rech_loc,no_wells,con_aquifer),3),head)
})
test_that("get_flowdir works for \"D\" recharge, confined",{
  expect_equal(round(get_flowdir(rech_loc,no_wells,con_aquifer),3),df)
})

un_aquifer <- define_aquifer("unconfined",1,h0=10,z0=1,recharge=recharge_params)
head <- c(10,10.1,10.198,10.1,10.198,10.1,10.198,10.1,10) # round(get_hydraulic_head(rech_loc,no_wells,con_aquifer),3) %>% paste(collapse=",")
test_that("get_hydraulic_head works for \"D\" recharge, confined",{
  expect_equal(round(get_hydraulic_head(rech_loc,no_wells,un_aquifer),3),head)
})
df <- data.frame(dx=c(-0.1,-0.099,0,-0.099,0,0.099,0,0.099,0.1),
                 dy=c(-0.1,-0.099,0,-0.099,0,0.099,0,0.099,0.1))
test_that("get_flowdir works for \"D\" recharge, unconfined",{
  expect_equal(round(get_flowdir(rech_loc,no_wells,un_aquifer),3),df)
})
### END TEST RECHARGE

# Create a grid of locations and define aquifer
loc <- tidyr::crossing(x=seq(-200,200,length.out=201),y=seq(-200,200,length.out=201))
aquifer <- define_aquifer("confined",1e-4,z0=20,h0=0)

# No flow boundary
wells_no_flow <- define_wells(x=c(-100,100),y=c(-0,0),Q=c(-1e-2,-1e-2),diam=c(0.1,0.1),R=c(500,500))
no_flow_boundary <- loc %>%
  dplyr::bind_cols(streamfunction=get_stream_function(loc,wells_no_flow,aquifer)) %>%
  dplyr::bind_cols(head=get_hydraulic_head(loc,wells_no_flow,aquifer))
test_that("get_stream_function accurately models no-flow boundary along boundary",{
  expect_equal(no_flow_boundary %>% dplyr::filter(x==0) %>% purrr::pluck("streamfunction") %>% table() %>% length(),2)
})
test_that("get_stream_function accurately models no-flow boundary perpendicular to boundary and intersecting wells",{
  expect_equal(no_flow_boundary %>% dplyr::filter(y==0) %>% purrr::pluck("streamfunction") %>% table() %>% length(),3)
})

# Constant head boundary
wells_constant_head <- define_wells(x=c(-100,100),y=c(-0,0),Q=c(1e-2,-1e-2),diam=c(0.1,0.1),R=c(500,500))
aquifer <- define_aquifer("confined",1e-4,z0=20,h0=0)
constant_head_boundary <- loc %>%
  dplyr::bind_cols(streamfunction=get_stream_function(loc,wells_constant_head,aquifer))
test_that("get_stream_function accurately models constant-head boundary perpendicular to boundary and intersecting wells",{
  expect_equal(constant_head_boundary %>% dplyr::filter(y==0) %>% purrr::pluck("streamfunction") %>% table() %>% length(),2)
})
