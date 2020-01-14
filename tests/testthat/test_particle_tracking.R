# test_particle_tracking.R

# define boundaries, aquifer, and wells
bounds_df <- data.frame(bound_type=c("NF","NF","CH","NF"),m=c(Inf,0,Inf,0),b=c(0,1000,1000,0))
aquifer <- define_aquifer(aquifer_type="confined",Ksat=0.001,n=0.4,h0=0,z0=20,bounds=bounds_df)
wells <- data.frame(x=c(400,100,650),y=c(300,600,800),Q=c(-1e-1,-1e-1,1e-1),diam=c(1,1,1),R=c(500,100,600)) %>%
  define_wells() %>% generate_image_wells(aquifer)

# track particle to a well
particle_path <- track_particle(c(600,500), wells, aquifer)
test_that("track_particle tracks a particle to a well",{
  expect_equal(particle_path[nrow(particle_path),] %>% dplyr::mutate_at(c("time_days","x","y"),round),
               tibble::tibble(time_days=167,x=401,y=300,status="Reached well"))
})

# track particle to a boundary
particle_path <- track_particle(c(995,800), wells, aquifer)
test_that("track_particle tracks a particle to a boundary",{
  expect_equal(particle_path[nrow(particle_path),] %>% dplyr::mutate_at(c("time_days","x","y"),round),
               tibble::tibble(time_days=4,x=1000,y=800,status="Reached boundary"))
})

# track particle -- max time reached
particle_path <- track_particle(c(725,825), wells, aquifer, t_max=100)
test_that("track_particle stops when max time is reached",{
  expect_equal(particle_path[nrow(particle_path),] %>% dplyr::mutate_at(c("time_days","x","y"),round),
               tibble::tibble(time_days=100,x=883,y=805,status="Max time reached"))
})

# track particle -- stops when not moving
particle_path <- track_particle(c(900,50), wells, aquifer)
test_that("track_particle stops when max time is reached",{
  expect_equal(particle_path[nrow(particle_path),] %>% dplyr::mutate_at(c("time_days","x","y"),round),
               tibble::tibble(time_days=0,x=900,y=50,status="Zero velocity"))
})
