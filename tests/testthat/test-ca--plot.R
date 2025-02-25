test_that("plot.erahumed_ca does not produce an error with valid inputs", {
  skip("plot.erahumed_ca() no longer used")
  ca_obj <- get_layer(test_sim_small(), "ca")
  cluster_id <- get_layer_output(ca_obj)$cluster_id[1]

  expect_no_error(plot(ca_obj, cluster_id = cluster_id))
})

test_that("plot.ca snapshot is constant", {
  skip("plot.erahumed_ca() no longer used")
  skip_on_ci()

  ca_obj <- get_layer(test_sim_small(), "ca")
  cluster_id <- get_layer_output(ca_obj)$cluster_id[1]
  plot_obj <- plot(ca_obj, cluster_id = cluster_id)

  expect_snapshot(plot_obj)
})
