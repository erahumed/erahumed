test_that("No error with basic valid inputs", {
  rc_obj <- get_layer(test_sim_small(), "rc")
  cluster_id <- get_layer_output(rc_obj)$element_id[[1]]

  expect_no_error( plot(rc_obj, cluster_id = cluster_id) )
})
