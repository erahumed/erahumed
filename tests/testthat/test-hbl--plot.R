test_that("plot.hbl does not produce an error with valid inputs", {
  hbl_obj <- get_layer(test_sim_small(), "hbl")

  expect_no_error(plot(hbl_obj, "residence_time_days"))
})

test_that("plot.hbl raises an error with invalid inputs", {
  hbl_obj <- get_layer(test_sim_small(), "hbl")

  expect_error(plot(hbl_obj, "invalid_variable"), class = "plot.hbl_error")
  expect_error(plot(hbl_obj, variable = 840), class = "plot.hbl_error")
})

test_that("plot.hbl raises a warning with unused arguments", {
  hbl_obj <- get_layer(test_sim_small(), "hbl")

  expect_warning(plot(hbl_obj,
                      variable = "residence_time_days",
                      unused_argument = "argument"),
                 class = "plot.hbl_warning"
                 )
})

test_that("plot.hbl snapshot is constant", {
  skip_on_ci()

  hbl_obj <- get_layer(test_sim_small(), "hbl")
  plot_obj <- plot(hbl_obj, variable = "volume")

  expect_snapshot(plot_obj)
})
