test_that("get_results() succeeds", {
  sim <- test_sim_small()

  expect_no_error(get_results(sim, component = "hydrology", element = "lake"))
  expect_no_error(get_results(sim, component = "hydrology", element = "ditch"))
  expect_no_error(get_results(sim, component = "hydrology", element = "cluster"))
  expect_no_error(get_results(sim, component = "exposure", element = "lake"))
  expect_no_error(get_results(sim, component = "exposure", element = "ditch"))
  expect_no_error(get_results(sim, component = "exposure", element = "cluster"))
  expect_no_error(get_results(sim, component = "risk", element = "lake"))
  expect_no_error(get_results(sim, component = "risk", element = "ditch"))
  expect_no_error(get_results(sim, component = "risk", element = "cluster"))
})

test_that("get_results() returns data-frames", {
  sim <- test_sim_small()

  expect_s3_class(get_results(sim, component = "hydrology", element = "lake"), class = "data.frame")
  expect_s3_class(get_results(sim, component = "hydrology", element = "ditch"), class = "data.frame")
  expect_s3_class(get_results(sim, component = "hydrology", element = "cluster"), class = "data.frame")
  expect_s3_class(get_results(sim, component = "exposure", element = "lake"), class = "data.frame")
  expect_s3_class(get_results(sim, component = "exposure", element = "ditch"), class = "data.frame")
  expect_s3_class(get_results(sim, component = "exposure", element = "cluster"), class = "data.frame")
  expect_s3_class(get_results(sim, component = "risk", element = "lake"), class = "data.frame")
  expect_s3_class(get_results(sim, component = "risk", element = "ditch"), class = "data.frame")
  expect_s3_class(get_results(sim, component = "risk", element = "cluster"), class = "data.frame")
})
