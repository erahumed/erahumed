test_that("get_simulation_layer() succeeds with valid input", {
  model <- test_mod_small()
  expect_no_error( get_simulation_layer(model, "hba") )
})

test_that("get_simulation_layer() throws an error with invalid input", {
  model <- test_mod_small()
  expect_error( get_simulation_layer(model, "unexisting_layer") )
})

test_that("get_simulation_layer() returns NULL if layer not yet computed", {
  model <- erahumed_simulation()
  expect_null( get_simulation_layer(model, "hba") )
})
