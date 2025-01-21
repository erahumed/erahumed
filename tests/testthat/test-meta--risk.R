test_that("extract_risk() returns data.frames", {
  sim <- test_sim_small()

  skip("Not implemented")  # TODO
  expect_no_error(extract_hydrology(sim, "lake"))
  expect_s3_class(extract_hydrology(sim, "lake"), "data.frame")
  expect_no_error(extract_hydrology(sim, "ditch"))
  expect_s3_class(extract_hydrology(sim, "ditch"), "data.frame")
  expect_no_error(extract_hydrology(sim, "cluster"))
  expect_s3_class(extract_hydrology(sim, "cluster"), "data.frame")
})
