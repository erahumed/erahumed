test_that("plot.hba does not produce an error with valid inputs", {
  df <- hba()

  expect_no_error(plot(df, "residence_time_days"))
})

test_that("plot.hba raises an error with invalid inputs", {
  df <- hba()
  expect_error(plot(df, "invalid_variable"), class = "plot.hba_error")
  expect_error(plot(df, variable = 840), class = "plot.hba_error")
})

test_that("plot.hba raises a warning with unused arguments", {
  df <- hba()
  expect_warning(plot(df,
                      variable = "residence_time_days",
                      unused_argument = "argument"),
                 class = "plot.hba_warning"
                 )
})

