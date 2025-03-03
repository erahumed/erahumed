test_that("plot.hbl does not produce an error with valid inputs", {
  hbl_obj <- get_layer(test_sim_small(), "hbl")

  expect_no_error(plot(hbl_obj))
})

test_that("plot.hbl snapshot is constant", {
  skip_on_ci()

  hbl_obj <- get_layer(test_sim_small(), "hbl")
  plot_obj <- plot(hbl_obj)

  expect_snapshot(plot_obj)
})
