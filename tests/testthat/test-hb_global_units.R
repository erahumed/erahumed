test_that("linear_petp_surface() throws no errors on regular call", {
  expect_no_error( linear_petp_surface(surface_P = 1, surface_ETP = 1) )
})

test_that("linear_petp_surface() returns a closure", {
  res <- linear_petp_surface(surface_P = 1, surface_ETP = 1)
  expect_type(res, "closure")
})

test_that("return of linear_petp_surface() can be called with two arguments", {
  res <- linear_petp_surface(surface_P = 1, surface_ETP = 1)
  expect_no_error(res(0.5, 0.4))
})

test_that("linear_petp_surface() returns the correct affine function", {
  sP <- .95
  sETP <- .85

  P <- c(1, 2, 3)
  ETP <- c(3, 2, 1)

  res <- linear_petp_surface(surface_P = sP, surface_ETP = sETP)
  expect_equal(
    res(P = c(1, 2, 3), ETP = c(3, 2, 1)),
    1e-3 * (sP * P - ETP * sETP)
  )
})


test_that("linear_storage_curve() throws no errors on regular call", {
  expect_no_error( linear_storage_curve(intercept = 0, slope = 1) )
})

test_that("linear_storage_curve() returns a closure", {
  res <- linear_storage_curve(intercept = 0, slope = 1)
  expect_type(res, "closure")
})

test_that("return of linear_storage_curve() can be called with one argument", {
  res <- linear_storage_curve(intercept = 0, slope = 1)
  expect_no_error(res(1))
})

test_that("linear_storage_curve() returns the correct affine function", {
  slope <- 4
  intercept <- 3
  level <- 1:10
  res <- linear_storage_curve(intercept = intercept, slope = slope)
  expect_identical(res(level), slope * level + intercept)
})



test_that("residence_time throws no errors on regular call", {
  volume <- c(1, 0.9, 0.86, 0.93, 1.1)
  inflow <- c(0.1, 0.12, 0.2, 0.5, 0.2)

  expect_no_error(hbg_residence_time(volume, inflow))
})

test_that("residence_time 'k' argument controls level of smoothing", {
  volume <- c(1, 0.9, 0.86, 0.93, 1.1)
  inflow <- c(0.1, 0.12, 0.2, 0.5, 0.2)
  k <- 3

  expect_identical(
    hbg_residence_time(volume, inflow, k = k),
    moving_average(volume, k = k) / moving_average(inflow, k = k) / s_per_day()
  )
})
