# test_wrappers.R

# define aquifer
bounds_df <- data.frame(bound_type=c("CH","NF","NF","NF"),m=c(Inf,0,Inf,0),b=c(0,1000,1000,0))
aquifer_unconfined <- define_aquifer("unconfined",1e-3,bounds=bounds_df,h0=100)

# define wells and well images
set.seed(30)
wells_df <- data.frame(x=runif(8,0,1000),y=runif(8,0,1000),diam=1) %>%
  dplyr::mutate(R=get_ROI(Ksat=aquifer_unconfined$Ksat,h=aquifer_unconfined$h0,t=3600*24*365,n=0.4,method="aravin-numerov"),  # t = 1 year
                country=factor(y>500,levels=c(F,T),labels=c("A","B")),
                weights=1)
wells <- define_wells(wells_df) %>% generate_image_wells(aquifer_unconfined)
df <- tibble::tibble(var=c("PHI_A_A", "PHI_A_B", "PHI_B_A", "PHI_B_B"),
                 pot=round(c(1317.4124,242.8351,242.8351,875.3999),5))
test_that("get_pumping_relationships returns correct result for simple aquifer and two countries",{
  expect_equal(get_drawdown_relationships(wells,aquifer_unconfined,country,weights) %>% dplyr::mutate(pot=round(pot,4)) %>% dplyr::select(var,pot),df)
})


# define aquifer
bounds_df <- data.frame(bound_type=c("CH","NF","NF","NF"),m=c(Inf,0,Inf,0),b=c(0,1000,1000,0))
aquifer_unconfined <- define_aquifer("unconfined",1e-3,bounds=bounds_df,h0=100)

# define wells and well images
set.seed(30)
wells_df <- data.frame(x=runif(8,0,1000),y=runif(8,0,1000),diam=1) %>% dplyr::mutate(R=1000,Q=-1/dplyr::n())
wells <- define_wells(wells_df) %>% generate_image_wells(aquifer_unconfined)
gridded <- get_gridded_hydrodynamics(wells,aquifer_unconfined,c(15,15),c(8,8))
test_that("get_gridded_hydrodynamics returns accurate dimensions for head",{
  expect_equal(dim(gridded$head),c(225,3))
})
test_that("get_gridded_hydrodynamics returns accurate dimensions for flow",{
  expect_equal(dim(gridded$flow),c(64,6))
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
test_that("use_anem_function works for get_utm_rectangle",{
  expect_equal(use_anem_function("get_utm_rectangle",edges_user=edges_user) %>%
                 dplyr::mutate_if(is.numeric,function(x) round(x,2)),edges_rect)
})



