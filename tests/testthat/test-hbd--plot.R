test_that("No error with basic valid inputs", {
  plot_hbd(test_sim_small(), element_id = info_ditches()$element_id[1]) |>
    expect_no_error()
})

test_that("No error for all plot types and variables", {
  common_args <- list(simulation = test_sim_small(),
                      element_id = info_ditches()$element_id[1])
  args1 <- c(common_args, type = "storage", variable = "volume")
  args2 <- c(common_args, type = "storage", variable = "depth")
  args3 <- c(common_args, type = "flows", variable = "volume")
  args4 <- c(common_args, type = "flows", variable = "depth")

  expect_no_error( do.call(plot_hbd, args1) )
  expect_no_error( do.call(plot_hbd, args2) )
  expect_no_error( do.call(plot_hbd, args3) )
  expect_no_error( do.call(plot_hbd, args4) )
})

test_that("Succeeds but warns if no ditch is specified", {
  plot_hbd(test_sim_small()) |>
    expect_no_error() |>
    expect_warning()
})

test_that("plot_hbc snapshot is constant", {
  plot_hbd(test_sim_small(),
           type = "storage",
           variable = "volume",
           element_id = info_ditches()$element_id[1]
  ) |>
    expect_snapshot()
})
