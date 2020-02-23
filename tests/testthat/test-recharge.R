# test_recharge.R

#### CONSTANT FLOW RECHARGE

# 1a: Confined aquifer - constant flow "F" recharge
recharge_params <- list(recharge_type="F",recharge_vector=c(0,0,1,1),flow=1,x0=0,y0=0)
con_aquifer <- define_aquifer("confined",1,h0=0,z0=1,recharge=recharge_params)
test_that(paste("get_recharge_undisturbed_potential: numeric loc and confined aquifer, \"F\" recharge"),{
  expect_equal(get_recharge_undisturbed_potential(c(1/sqrt(2),1/sqrt(2)), con_aquifer),-1)
})

loc <- expand.grid(x=-2:2,y=-2:2)
head_loc <- c(2.8284,2.1213,1.4142,0.7071,0,2.1213,1.4142,0.7071,0,-0.7071,1.4142,0.7071,0,
       -0.7071,-1.4142,0.7071,0,-0.7071,-1.4142,-2.1213,0,-0.7071,-1.4142,-2.1213,-2.8284)
test_that(paste("get_recharge_undisturbed_potential: df loc and confined aquifer, \"F\" recharge"),{
  expect_equal(round(get_recharge_undisturbed_potential(loc, con_aquifer),4),head_loc)
})

# 1b: Unconfined aquifer - constant flow "F" recharge
recharge_params <- list(recharge_type="F",recharge_vector=c(0,0,1,1),flow=1,x0=0,y0=0)
un_aquifer <- define_aquifer("unconfined",1,h0=50,recharge=recharge_params)

test_that(paste("get_recharge_undisturbed_potential: numeric loc and unconfined aquifer, \"F\" recharge"),{
  expect_equal(get_recharge_undisturbed_potential(c(1/sqrt(2),1/sqrt(2)), un_aquifer),2498)
})

loc <- expand.grid(x=-2:2,y=-2:2)
pot_loc <- c(2505.6569,2504.2426,2502.8284,2501.4142,2500,2504.2426,2502.8284,2501.4142,
              2500,2498.5858,2502.8284,2501.4142,2500,2498.5858,2497.1716,2501.4142,2500,
              2498.5858,2497.1716,2495.7574,2500,2498.5858,2497.1716,2495.7574,2494.3431)
test_that(paste("get_recharge_undisturbed_potential: df loc and unconfined aquifer, \"F\" recharge"),{
  expect_equal(round(get_recharge_undisturbed_potential(loc, un_aquifer),4),pot_loc)
})

#### DIVIDE RECHARGE

# 2a: Confined aquifer - constant flow "D" recharge
recharge_params <- list(recharge_type="D",recharge_vector=c(0,0,1,sqrt(3)),flow_main=1,flow_opp=2,x0=0,y0=0)
con_aquifer <- define_aquifer("confined",1,h0=0,z0=1,recharge=recharge_params)
test_that(paste("get_recharge_undisturbed_potential: numeric loc and confined aquifer, \"D\" recharge"),{
  expect_equal(get_recharge_undisturbed_potential(c(1/2,sqrt(3)/2), con_aquifer),-1)
  expect_equal(get_recharge_undisturbed_potential(c(-1/2,-sqrt(3)/2), con_aquifer),-2)
})

loc <- expand.grid(x=-1:1,y=-1:1)
head_loc <- c(-2.7321,-1.7321,-0.7321,-1,0,-0.5,-0.366,-0.866,-1.366)
test_that(paste("get_recharge_undisturbed_potential: df loc and confined aquifer, \"D\" recharge"),{
  expect_equal(round(get_recharge_undisturbed_potential(loc, con_aquifer),4),head_loc)
})

# 2a-ii: Confined aquifer - constant flow "D" recharge, m = Inf
recharge_params <- list(recharge_type="D",recharge_vector=c(1,1,2,1),flow_main=1,flow_opp=2,x0=1,y0=0)
con_aquifer <- define_aquifer("confined",1,h0=0,z0=1,recharge=recharge_params)
test_that(paste("get_recharge_undisturbed_potential: numeric loc and confined aquifer, \"D\" recharge, m = Inf"),{
  expect_equal(get_recharge_undisturbed_potential(c(-0,2), con_aquifer),-2)
  expect_equal(get_recharge_undisturbed_potential(c(1,0), con_aquifer),0)
  expect_equal(get_recharge_undisturbed_potential(c(2,0), con_aquifer),-1)
})

loc <- expand.grid(x=0:2,y=0:2)
head_loc <- c(-2,0,-1,-2,0,-1,-2,0,-1)
test_that(paste("get_recharge_undisturbed_potential: df loc and confined aquifer, \"D\" recharge, m = Inf"),{
  expect_equal(round(get_recharge_undisturbed_potential(loc, con_aquifer),4),head_loc)
})

# 2b-i: Unconfined aquifer - constant flow "D" recharge
recharge_params <- list(recharge_type="D",recharge_vector=c(0,0,1,1),flow_main=1,flow_opp=2,x0=0,y0=0)
un_aquifer <- define_aquifer("unconfined",1,h0=50,recharge=recharge_params)
test_that(paste("get_recharge_undisturbed_potential: numeric loc and unconfined aquifer, \"D\" recharge"),{
  expect_equal(get_recharge_undisturbed_potential(c(1/sqrt(2),1/sqrt(2)), un_aquifer),2498)
  expect_equal(get_recharge_undisturbed_potential(c(-1/sqrt(2),-1/sqrt(2)), un_aquifer),2496)
})

loc <- expand.grid(x=-1:1,y=-1:1)
pot_loc <- c(2494.3431,2497.1716,2500,2497.1716,2500,2498.5858,2500,2498.5858,2497.1716)
test_that(paste("get_recharge_undisturbed_potential: df loc and unconfined aquifer, \"D\" recharge"),{
  expect_equal(round(get_recharge_undisturbed_potential(loc, un_aquifer),4),pot_loc)
})


# 2b-ii: Unconfined aquifer - constant flow "D" recharge with m = Inf
recharge_params <- list(recharge_type="D",recharge_vector=c(10,10,11,10),flow_main=1,flow_opp=2,x0=10,y0=0)
un_aquifer <- define_aquifer("unconfined",1,h0=50,recharge=recharge_params)
test_that(paste("get_recharge_undisturbed_potential: numeric loc and unconfined aquifer, \"D\" recharge, m = Inf"),{
  expect_equal(get_recharge_undisturbed_potential(c(11,12), un_aquifer),2498)
  expect_equal(get_recharge_undisturbed_potential(c(7,-10), un_aquifer),2488)
  expect_equal(get_recharge_undisturbed_potential(c(10,-10), un_aquifer),2500)
})

loc <- expand.grid(x=9:11,y=9:11)
pot_loc <- c(2496,2500,2498,2496,2500,2498,2496,2500,2498)
test_that(paste("get_recharge_undisturbed_potential: df loc and unconfined aquifer, \"D\" recharge, m = Inf"),{
  expect_equal(round(get_recharge_undisturbed_potential(loc, un_aquifer),4),pot_loc)
})


