# test_shiny_helpers.R

edges_4 <- data.frame(x1=c(0,0,1,1),y1=c(0,1,1,0),
                      id=c(1,2,3,4), x2=c(0,1,1,0), y2=c(1,1,0,0))
test_that("get_edges_from_vertices works for 2, 3, and 4 vertices",{
  expect_equal(get_edges_from_vertices(data.frame(x=c(0,0),y=c(0,1),id=1:2)),edges_4[1,])
  expect_equal(get_edges_from_vertices(data.frame(x=c(0,0,1),y=c(0,1,1),id=1:3)),edges_4[1:2,])
  expect_equal(get_edges_from_vertices(data.frame(x=c(0,0,1,1),y=c(0,1,1,0),id=1:4)),edges_4)
})
test_that("get_edges_from_vertices returns no rows for 1 vertice",{
  expect_equal(get_edges_from_vertices(data.frame(x=c(0),y=c(0),id=1)),edges_4[FALSE,])
})
