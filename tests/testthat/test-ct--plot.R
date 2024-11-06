test_that("No error with basic valid inputs", {
  ct_obj <- get_simulation_layer(test_sim_small(), "ct")
  cluster_id <- layer_output(ct_obj)$cluster_id[[1]]

  expect_no_error( plot(ct_obj, cluster_id = cluster_id) )
})

test_that("No error with variable = 'density'", {
  ct_obj <- get_simulation_layer(test_sim_small(), "ct")
  cluster_id <- layer_output(ct_obj)$cluster_id[[1]]

  expect_no_error( plot(ct_obj, variable = "density", cluster_id = cluster_id) )
})
