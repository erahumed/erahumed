test_that("plot.hba does not produce an error with valid inputs", {
  hba_obj <- get_layer(test_sim_small(), "hba")

  expect_no_error(plot(hba_obj, "residence_time_days"))
})

test_that("plot.hba raises an error with invalid inputs", {
  hba_obj <- get_layer(test_sim_small(), "hba")

  expect_error(plot(hba_obj, "invalid_variable"), class = "plot.hba_error")
  expect_error(plot(hba_obj, variable = 840), class = "plot.hba_error")
})

test_that("plot.hba raises a warning with unused arguments", {
  hba_obj <- get_layer(test_sim_small(), "hba")

  expect_warning(plot(hba_obj,
                      variable = "residence_time_days",
                      unused_argument = "argument"),
                 class = "plot.hba_warning"
                 )
})

