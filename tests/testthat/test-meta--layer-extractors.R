test_that("get_layer() succeeds with valid arguments", {
  sim <- test_sim_small()
  expect_no_error(get_layer(sim, "hbc"))
})

test_that("get_layer() throws an error if no layer is specified", {
  sim <- test_sim_small()
  expect_error(get_layer(sim))
})

test_that("get_layer() throws an error for invalid 'layer' argument", {
  simulation <- test_sim_small()
  expect_error( get_layer(simulation, "unexisting_layer") )
})
