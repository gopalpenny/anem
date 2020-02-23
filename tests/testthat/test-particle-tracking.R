# test_particle_tracking.R

library(magrittr)

# define boundaries, aquifer, and wells
bounds_df <- data.frame(bound_type=c("NF","NF","CH","NF"),m=c(Inf,0,Inf,0),b=c(0,1000,1000,0))
aquifer <- define_aquifer(aquifer_type="confined",Ksat=0.001,n=0.4,h0=0,z0=20,bounds=bounds_df)
wells <- data.frame(x=c(400,100,650),y=c(300,600,800),Q=c(-1e-1,-1e-1,1e-1),diam=c(1,1,1),R=c(500,100,600)) %>%
  define_wells() %>% generate_image_wells(aquifer)

# track particle to a well
particle_path <- track_particles(c(600,500), wells, aquifer)
test_that("track_particles tracks a particle to a well",{
  expect_equal(particle_path[nrow(particle_path),] %>% dplyr::mutate_at(c("time_days","x","y"),function(x) floor(x/10)*10),
               tibble::tibble(time_days=160,x=400,y=300,status="Reached well",i=1L))
})

# track particle to a boundary
particle_path <- track_particles(c(800,800), wells, aquifer)
test_that("track_particles tracks a particle to a boundary",{
  expect_equal(particle_path[nrow(particle_path),] %>% dplyr::mutate_at(c("time_days","x","y"),function(x) round(x/10)*10),
               tibble::tibble(time_days=150,x=1000,y=760,status="Reached boundary",i=1L))
})

# track particle -- max time reached
particle_path <- track_particles(c(725,825), wells, aquifer, t_max=100)
test_that("track_particles stops when max time is reached",{
  expect_equal(particle_path[nrow(particle_path),] %>% dplyr::mutate_at(c("time_days","x","y"),round),
               tibble::tibble(time_days=100,x=884,y=805,status="Max time reached",i=1L))
})

# track particle -- stops when not moving
particle_path <- track_particles(c(900,50), wells, aquifer)
test_that("track_particles stops when max time is reached",{
  expect_equal(particle_path[nrow(particle_path),] %>% dplyr::mutate_at(c("time_days","x","y"),round),
               tibble::tibble(time_days=365,x=900,y=50,status="Zero velocity",i=1L))
})


loc <- data.frame(x=c(600,725,900,250,150,200),y=c(500,825,50,500,800,700)) %>% dplyr::mutate(p=letters[dplyr::row_number()])
particle_path <- track_particles(loc, wells, aquifer, t_max=365*100)
df <- tibble::tibble(time_days=c(168.228233320727, 184.090583379394, 365, 183.736995132244, 721.359234526391, 124.43217938518),
                 x=c(405.085736653523, 1001.27414978609, 900, 397.139501313506, 44.4723618090454, 104.322027607873),
                 y=c(302.825324519888, 796.898134148722, 50, 301.414172955994, 799.999991360473, 601.724299759824),
                 status=c("Reached well", "Reached boundary", "Zero velocity", "Reached well", "Zero velocity", "Reached well"),
                 i=c(1:6),
                 p=c("a", "b", "c", "d", "e", "f")) %>% dplyr::mutate_if(is.numeric,round)
test_that("track_particle works for data.frame loc input",{
  expect_equal(particle_path %>% dplyr::filter(status!="On path") %>% dplyr::mutate_if(is.numeric,round),df)
})
