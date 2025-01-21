test_that("extract_hydrology() returns data.frames", {
  sim <- test_sim_small()

  expect_no_error(extract_hydrology(sim, "lake"))
  expect_s3_class(extract_hydrology(sim, "lake"), "data.frame")
  expect_no_error(extract_hydrology(sim, "ditch"))
  expect_s3_class(extract_hydrology(sim, "ditch"), "data.frame")
  expect_no_error(extract_hydrology(sim, "cluster"))
  expect_s3_class(extract_hydrology(sim, "cluster"), "data.frame")
})
