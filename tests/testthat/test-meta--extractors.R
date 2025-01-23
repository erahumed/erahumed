test_that("get_results() succeeds in simple case", {
  sim <- test_sim_small()

  expect_no_error(get_results(sim, component = "hydrology", element = "lake"))
})
