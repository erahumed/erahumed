test_that("get_simulation_layer() succeeds with valid input", {
  simulation <- test_sim_small()
  expect_no_error( get_simulation_layer(simulation, "hba") )
})

test_that("get_simulation_layer() throws an error with invalid input", {
  simulation <- test_sim_small()
  expect_error( get_simulation_layer(simulation, "unexisting_layer") )
})

