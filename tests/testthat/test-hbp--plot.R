test_that("plot.hbc() succeeds w/ type='cluster_levels' and valid input", {
  hbc_obj <- get_layer(test_sim_small(), "hbc")
  cluster_id <- get_layer_output(hbc_obj)$cluster_id[1]
  expect_no_error(
    plot(hbc_obj, type = "cluster_view", cluster_id = cluster_id)
  )
})

test_that("plot.hbc() error w/ type='map'", {
  hbc_obj <- get_layer(test_sim_small(), "hbc")
  expect_error( plot(hbc_obj, type = "map_view") )
})

test_that("plot.hbc throws an error if no cluster is specified", {
  skip("This is no longer the actual behaviour")
  hbc_obj <- get_layer(test_sim_small(), "hbc")
  expect_error( plot(hbc_obj, type = "cluster_view") )  # No cluster_id passed
})

test_that("plot.hbc snapshot is constant", {
  skip_on_ci()

  hbc_obj <- get_layer(test_sim_small(), "hbc")
  cluster_id <- get_layer_output(hbc_obj)$cluster_id[1]
  plot_obj <- plot(hbc_obj, type = "cluster_view", cluster_id = cluster_id)

  expect_snapshot(plot_obj)
})
