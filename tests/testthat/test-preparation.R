# test_assess.R

bounds_df1 <- data.frame(bound_type=c("CH","NF","NF","NF"),x1=c(0,10,13,1),y1=c(0,10,9,-1),x2=c(10,13,1,0),y2=c(10,9,-1,0))
df1 <- round(tibble::tibble(bID=1:4,
                 m=c(0.909090909090909, -1.1, 0.909090909090909, -1.1),
                 b=c(0.454545454545455, 22.15, -2.36363636363636, 0.05),
                 x1=c(10.7986425339, 10.7986425339, 12.2013574661, -0.2013574661),
                 y1=c(10.2714932127, 10.2714932127, 8.7285067873, 0.2714932127),
                 x2=c(-0.2013574661, 12.2013574661, 1.2013574661, 1.2013574661),
                 y2=c(0.2714932127, 8.7285067873, -1.2714932127, -1.2714932127)),5)
test_that("define_bounds returns correct bounds for irregular quadrangle",{
  expect_equal(round(define_bounds(bounds_df1) %>% dplyr::select(-bound_type),5),df1)
})


bounds_df2 <- data.frame(bound_type=c("CH","CH","NF","NF"),x1=c(0,0,10,10),y1=c(0,10,10,0),x2=c(0,10,10,0),y2=c(10,10,0,0))
df2 <- tibble::tibble(bID=1:4,
                     bound_type=c("CH", "CH", "NF", "NF"),
                     m=c(Inf, 0, Inf, 0),
                     b=c(0, 10, 10, 0),
                     x1=c(0, 0, 10, 0),
                     y1=c(10, 10, 10, 0),
                     x2=c(0, 10, 10, 10),
                     y2=c(0, 10, 0, 0))
# define_bounds(bounds_df2) %>% print_data_frame_for_entry()
test_that("define_bounds returns correct bounds for vertical/horizontal square",{
  expect_equal(define_bounds(bounds_df2),df2)
})
