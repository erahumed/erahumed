test_that("linear_petp_surface throws no errors on regular call", {
  expect_no_error( linear_petp_surface(surface_P = 1, surface_ETP = 1) )
})

test_that("linear_storage_curve throws no errors on regular call", {
  expect_no_error( linear_storage_curve(intercept = 0, slope = 1) )
})

test_that("residence_time throws no errors on regular call", {
  volume <- c(1, 0.9, 0.86, 0.93, 1.1)
  inflow <- c(0.1, 0.12, 0.2, 0.5, 0.2)

  expect_no_error(residence_time(volume, inflow))
})
