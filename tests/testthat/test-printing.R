# test-printing.R

print_aquifer_output <- capture.output(print(aquifer_confined_example))

test_that("print.aquifer includes aquifer_type, Ksat, h0, z0",{
  expect_equal(any(grepl("aquifer_type",print_aquifer_output)),TRUE)
  expect_equal(any(grepl("Ksat",print_aquifer_output)),TRUE)
  expect_equal(any(grepl("h0",print_aquifer_output)),TRUE)
  expect_equal(any(grepl("z0",print_aquifer_output)),TRUE)
})
