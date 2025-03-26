test_that("No error with basic valid inputs", {
  element_id <- info_ditches()$ditch[[1]]

  expect_no_error( plot_rd(test_sim_small(), element_id = element_id) )
})
