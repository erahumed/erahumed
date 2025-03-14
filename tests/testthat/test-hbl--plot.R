test_that("No error with basic valid inputs", {
  expect_no_error( plot_hbl(test_sim_small()) )
})

test_that("No error for all plot types and variables", {
  common_args <- list(simulation = test_sim_small())
  args1 <- c(common_args, type = "storage", variable = "volume")
  args2 <- c(common_args, type = "storage", variable = "depth")
  args3 <- c(common_args, type = "flows", variable = "volume")
  args4 <- c(common_args, type = "flows", variable = "depth")

  expect_no_error( do.call(plot_hbl, args1) )
  expect_no_error( do.call(plot_hbl, args2) )
  expect_no_error( do.call(plot_hbl, args3) )
  expect_no_error( do.call(plot_hbl, args4) )
})

test_that("plot_hbl snapshot is constant", {
  plot_hbl(test_sim_small(), type = "storage", variable = "volume") |>
    expect_snapshot()
})
