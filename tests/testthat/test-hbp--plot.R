test_that("plot.hbp() succeeds w/ type='cluster_levels' and valid input", {
  hbp_obj <- get_layer(test_sim_small(), "hbp")
  cluster_id <- get_layer_output(hbp_obj)$cluster_id[1]
  expect_no_error(
    plot(hbp_obj, type = "cluster_view", cluster_id = cluster_id)
  )
})

test_that("plot.hbp() error w/ type='map'", {
  hbp_obj <- get_layer(test_sim_small(), "hbp")
  expect_error( plot(hbp_obj, type = "map_view") )
})

test_that("plot.hbp throws an error if no cluster is specified", {
  hbp_obj <- get_layer(test_sim_small(), "hbp")
  expect_error( plot(hbp_obj, type = "cluster_view") )  # No cluster_id passed
})

test_that("plot.hbp snapshot is constant", {
  skip_on_ci()

  hbp_obj <- get_layer(test_sim_small(), "hbp")
  cluster_id <- get_layer_output(hbp_obj)$cluster_id[1]
  plot_obj <- plot(hbp_obj, type = "cluster_view", cluster_id = cluster_id)

  expect_snapshot(plot_obj)
})
