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
                 dplyr::group_by(bID) %>% dplyr::summarize(flow_normal=sum(flow_normal)) %>% purrr::pluck(2) %>% round(6),
               c(0.000519,-0.000543,-0.000381, 0.000484))
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
