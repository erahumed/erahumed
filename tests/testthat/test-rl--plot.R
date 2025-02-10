test_that("No error with basic valid inputs", {
  rl_obj <- get_layer(test_sim_small(), "rl")
  expect_no_error( plot(rl_obj) )
})
