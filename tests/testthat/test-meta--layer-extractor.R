test_that("get_simulation_layer() succeeds with valid input", {
  simulation <- test_mod_small()
  expect_no_error( get_simulation_layer(simulation, "hba") )
})

test_that("get_simulation_layer() throws an error with invalid input", {
  simulation <- test_mod_small()
  expect_error( get_simulation_layer(simulation, "unexisting_layer") )
})

test_that("get_simulation_layer() returns NULL if layer not yet computed", {
  simulation <- erahumed_simulation()
  expect_null( get_simulation_layer(simulation, "hba") )
})
