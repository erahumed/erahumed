obj <- erahumed_simulation() |>
  compute_inp() |>
  compute_hba() |>
  hba()

test_that("plot.hba does not produce an error with valid inputs", {

  expect_no_error(plot(obj, "residence_time_days"))
})

test_that("plot.hba raises an error with invalid inputs", {
  expect_error(plot(obj, "invalid_variable"), class = "plot.hba_error")
  expect_error(plot(obj, variable = 840), class = "plot.hba_error")
})

test_that("plot.hba raises a warning with unused arguments", {
  expect_warning(plot(obj,
                      variable = "residence_time_days",
                      unused_argument = "argument"),
                 class = "plot.hba_warning"
                 )
})

