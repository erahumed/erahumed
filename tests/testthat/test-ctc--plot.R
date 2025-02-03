test_that("No error with basic valid inputs", {
  ct_obj <- get_layer(test_sim_small(), "ctc")
  cluster_id <- get_layer_output(ct_obj)$element_id[[1]]

  expect_no_error( plot(ct_obj, cluster_id = cluster_id) )
})

test_that("No error with variable = 'density'", {
  ct_obj <- get_layer(test_sim_small(), "ctc")
  cluster_id <- get_layer_output(ct_obj)$element_id[[1]]

  expect_no_error( plot(ct_obj, variable = "density", cluster_id = cluster_id) )
})

test_that("plot.ctc snapshot is constant", {
  skip_on_ci()

  ct_obj <- get_layer(test_sim_small(), "ctc")
  cluster_id <- get_layer_output(ct_obj)$element_id[1]
  plot_obj <- plot(ct_obj, cluster_id = cluster_id)

  expect_snapshot(plot_obj)
})
