test_that("No error with basic valid inputs", {
  expect_no_error( plot_ctl(test_sim_small()) )
})

test_that("No error for all plot types and variables", {
  expect_no_error( plot_ctl(test_sim_small(), compartment = "water") )
  expect_no_error( plot_ctl(test_sim_small(), compartment = "sediment") )
})

test_that("Snapshot is constant", {
  expect_snapshot( plot_ctl(test_sim_small()) )
})
