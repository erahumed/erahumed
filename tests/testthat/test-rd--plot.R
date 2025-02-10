test_that("No error with basic valid inputs", {
  rd_obj <- get_layer(test_sim_small(), "rd")
  ditch <- get_layer_output(rd_obj)$element_id[[1]]

  expect_no_error( plot(rd_obj, ditch = ditch) )
})
