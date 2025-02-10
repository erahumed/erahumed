test_that("extract_exposure() returns data.frames", {
  sim <- test_sim_small()

  expect_no_error(extract_exposure(sim, "cluster"))
  expect_s3_class(extract_exposure(sim, "cluster"), "data.frame")
  expect_no_error(extract_exposure(sim, "lake"))
  expect_s3_class(extract_exposure(sim, "lake"), "data.frame")
  expect_no_error(extract_exposure(sim, "ditch"))
  expect_s3_class(extract_exposure(sim, "ditch"), "data.frame")
})
