## code to prepare `anem_scenarios` dataset goes here

library(tidyverse)

# define wells and well images
set.seed(30)
wells_example <- data.frame(x=runif(8,0,1000),y=runif(8,0,1000),diam=1) %>%
  mutate(R=1000,  # t = 1 year
         country=factor(y>500,levels=c(F,T),labels=c("A","B"))) %>%
  group_by(country) %>%
  mutate(weights=1,Q=-1/n()) %>% group_by()

bounds_example <- data.frame(bound_type=c("CH","NF","NF","NF"),m=c(Inf,0,Inf,0),b=c(0,1000,1000,0))
aquifer_example <- define_aquifer("unconfined",1e-3,bounds=bounds_example,h0=100,z0=10,n=0.35)

usethis::use_data(wells_example,bounds_example,aquifer_example,overwrite = TRUE)
