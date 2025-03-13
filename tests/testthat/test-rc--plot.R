test_that("No error with basic valid inputs", {
  rc_obj <- get_layer(test_sim_small(), "rc")
  element_id <- get_output(rc_obj)$element_id[[1]]

  expect_no_error( plot(rc_obj, element_id = element_id) )
})
