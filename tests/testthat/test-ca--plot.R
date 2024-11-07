test_that("plot.erahumed_ca does not produce an error with valid inputs", {
  ca_obj <- get_layer(test_sim_small(), "ca")
  cluster_id <- get_layer_output(ca_obj)$cluster_id[1]

  expect_no_error(plot(ca_obj, cluster_id = cluster_id))
})
