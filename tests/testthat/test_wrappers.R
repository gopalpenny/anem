# test_wrappers.R

# define aquifer
bounds_df <- data.frame(bound_type=c("CH","NF","NF","NF"),m=c(Inf,0,Inf,0),b=c(0,1000,1000,0))
aquifer_confined <- define_aquifer("unconfined",1e-3,bounds=bounds_df,h0=100)

# define wells and well images
set.seed(30)
wells_df <- data.frame(x=runif(8,0,1000),y=runif(8,0,1000),diam=1) %>%
  dplyr::mutate(R=get_ROI(Ksat=aquifer_confined$Ksat,h=aquifer_confined$h0,t=3600*24*365,n=0.4,method="aravin-numerov"),  # t = 1 year
                country=factor(y>500,levels=c(F,T),labels=c("A","B")),
                weights=1)
wells <- define_wells(wells_df) %>% generate_image_wells(aquifer_confined)
df <- tibble::tibble(var=c("PHI_A_A", "PHI_A_B", "PHI_B_A", "PHI_B_B"),
                 pot=round(c(8174.94368307291, 7287.70381951019, 7287.70381951019, 8676.87078600676),5))
test_that("get_pumping_relationships returns correct result for simple aquifer and two countries",{
  expect_equal(get_pumping_relationships(wells,aquifer_confined,country,weights) %>% dplyr::mutate(pot=round(pot,5)) %>% dplyr::select(var,pot),df)
})
