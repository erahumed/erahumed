test_that("No error with basic valid inputs", {
  plot_ctd(test_sim_small(), element_id = info_ditches()$element_id[1]) |>
    expect_no_error()
})

test_that("No error for all plot types and variables", {
  common_args <- list(simulation = test_sim_small(),
                      element_id = info_ditches()$element_id[1])

  expect_no_error( do.call(plot_ctd, c(common_args, compartment = "water")) )
  expect_no_error( do.call(plot_ctd, c(common_args, compartment = "sediment")) )
})

test_that("Succeeds but warns if no element_id is specified", {
  plot_ctd(test_sim_small()) |>
    expect_no_error() |>
    expect_warning()
})

test_that("Snapshot is constant", {
  plot_ctd(test_sim_small(), element_id = info_ditches()$element_id[1]) |>
    expect_snapshot()
})
