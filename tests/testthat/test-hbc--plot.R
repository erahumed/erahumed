test_that("plot.hbc() succeeds w/ type='cluster_levels' and valid input", {
  hbc_obj <- get_layer(test_sim_small(), "hbc")
  element_id <- get_output(hbc_obj)$cluster_id[1]
  expect_no_error(
    plot(hbc_obj, type = "storage", element_id = element_id)
  )
})

test_that("plot.hbc throws a warning if no cluster is specified", {
  hbc_obj <- get_layer(test_sim_small(), "hbc")
  expect_warning( plot(hbc_obj, type = "storage") )  # No cluster_id passed
})

test_that("plot.hbc snapshot is constant", {
  skip_on_ci()

  hbc_obj <- get_layer(test_sim_small(), "hbc")
  element_id <- get_output(hbc_obj)$cluster_id[1]
  plot_obj <- plot(hbc_obj, type = "storage", element_id = element_id)

  expect_snapshot(plot_obj)
})
