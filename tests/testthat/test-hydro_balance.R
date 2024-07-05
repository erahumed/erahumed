test_that("petp_volume_change throws no errors on regular call", {
  P <- c(1, 2, 3)
  ETP <- c(0.1, 0.2, 0.3)

  surface_P <- 1000
  surface_ETP <- 987

  expect_no_error(petp_volume_change(P, ETP, surface_P, surface_ETP))
})

test_that("albufera_storage_curve throws no errors on regular call", {
  level <- 1:10
  expect_no_error(albufera_storage_curve(level))
})

test_that("residence_time throws no errors on regular call", {
  volume <- c(1, 0.9, 0.86, 0.93, 1.1)
  inflow <- c(0.1, 0.12, 0.2, 0.5, 0.2)

  expect_no_error(residence_time(volume, inflow))
})
