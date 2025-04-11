test_that("No error with basic valid inputs", {
  element_id <- info_ditches()$element_id[[1]]

  expect_no_error( plot_rd(test_sim_small(), element_id = element_id) )
})

test_that("Succeeds but warns if no ditch is specified", {
  plot_rd(test_sim_small()) |>
    expect_no_error() |>
    expect_warning()
})
