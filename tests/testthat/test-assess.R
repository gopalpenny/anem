# test_assess.R


test_that("test assess working",{
  expect_equal(1,1)
})



library(magrittr)
bounds_df <- data.frame(bound_type=c("CH","NF","NF","NF"),m=c(Inf,0,Inf,0),b=c(0,1000,1000,0))
aquifer <- define_aquifer("unconfined",1e-3,bounds=bounds_df,h0=100)

set.seed(30)
wells_df <- data.frame(x=runif(8,0,1000),y=runif(8,0,1000),diam=1) %>%
  dplyr::mutate(R=1000,  #
         country=factor(y>500,levels=c(F,T),labels=c("A","B"))) %>%
  dplyr::group_by(country) %>%
  dplyr::mutate(weights=1,Q=-1/dplyr::n()) %>% dplyr::group_by()
wells_actual <- define_wells(wells_df)
wells <- wells_actual %>% generate_image_wells(aquifer)

test_that("get_bounds_behavior works shows no normal flow for NF boundaries and constant head for CH boundaries",{
  expect_equal(get_bounds_behavior(wells_actual,aquifer) %>%
                 dplyr::group_by(bID) %>% dplyr::summarize(flow_normal=sum(flow_normal)) %>% purrr::pluck(2) %>% round(5),
               c(0.00052,-0.00054,-0.00038, 0.00049)) # c(0.000519,-0.000543,-0.000381, 0.000484)) used for more precise Richardson approx of flow
})



gg_list <- plot_bounds_behavior(wells,aquifer,length.out=20)
# gridExtra::grid.arrange(gg_list$p_h,gg_list$p_f,nrow=1)
test_that("plot_bounds_behavior generates ggplots",{
  expect_equal(class(gg_list$p_h),c("gg","ggplot"))
  expect_equal(class(gg_list$p_f),c("gg","ggplot"))
})

test_that("plot_bounds_behavior generates 4x5 table",{
  expect_equal(dim(gg_list$table),c(4,5))
})

test_that("plot_bounds_behavior generates bounds_behavior",{
  expect_equal(dim(gg_list$bounds_behavior),c(160,15))
})

wells <- define_wells(x=c(50,10),y=c(25,2.5),Q=c(0.5,-0.2),diam=c(0.05,0.08)) %>%
  dplyr::mutate(R=get_ROI(Ksat=0.0000005,h=10,t=630720000,n=0.5,method="aravin-numerov"))  # 630720000 - 20 years
bounds_df <- tibble::tibble(bound_type=c("CH","NF","NF","NF"), m=c(0.5,-2,0.5,-2),b=c(0,100,20,50), bID=as.numeric(1:4))
aquifer <- define_aquifer("unconfined",1e-4,bounds=bounds_df,h0=100)
well_images <- generate_image_wells(wells,aquifer)
segments <- c(40,20,15,20)
segments_behavior <- get_segments_behavior(segments,well_images,aquifer) %>% tibble::as_tibble()
test_that("get_segments_behavior properly extracts head",{
  expect_equal(round(max(segments_behavior$head),4),103.9295)
  expect_equal(round(min(segments_behavior$head),4),100)
  expect_equal(round(mean(segments_behavior$head),4),102.1467)
})

test_that("get_segments_behavior properly extracts flow_normal",{
  expect_equal(round(max(segments_behavior$flow_normal*1e5),4),7.1926)
  expect_equal(round(min(segments_behavior$flow_normal*1e5),4),-3.8801)
  expect_equal(round(mean(segments_behavior$flow_normal*1e5),4),-0.202)
})
