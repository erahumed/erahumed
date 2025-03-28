test_that("ca_delay_vector(): output has same type and length of input", {
  set.seed(840)

  n <- 20
  inputs <- list(
    double = rnorm(n),
    integer = rpois(n, 100),
    logical = rnorm(n) > .5,
    character = letters[1:n]
    )
  delay <- cumsum(runif(n) > .8)

  for (input in inputs) {
    output <- ca_delay_vector(input, delay)
    expect_vector(output, ptype = input, size = length(input))
  }
})

test_that("ca_delay_vector(): output is correct in simple case", {
  x <-        1:10
  delay <-    c(0, 0, 0, 1, 1, 2, 3, 3, 3, 0 )
  expected <- c(1, 2, 3, 3, 4, 4, 4, 5, 6, 10)

  expect_equal(ca_delay_vector(x, delay), expected)
})



test_that("ca_filter_by_state(): output has correct type and length", {
  n <- 107

  value <- ca_filter_by_state(irrigation = logical(n),
                              draining = logical(n),
                              application_type = "ground")

  expect_vector(value, ptype = logical(), size = n)
})

test_that("ca_filter_by_state(): output is correct in simple case 1", {
  irrigation = c(T, T, F, F, T, F, T, F, T)
  draining = c(T, T, F, F, T, F, T, F, T)
  value <- ca_filter_by_state(irrigation,
                              draining,
                              application_type = "ground")
  expected <- (!irrigation) & (!draining)

  expect_equal(value, expected)
})

test_that("ca_filter_by_state(): output is correct in simple case 2", {
  irrigation = c(T, T, F, F, T, F, T, F, T)
  draining = c(T, T, F, F, T, F, T, F, T)
  value <- ca_filter_by_state(irrigation,
                              draining,
                              application_type = "aerial")
  expected <- irrigation & draining

  expect_equal(value, expected)
})



test_that("ca_filter_by_water_level(): output has correct type and length", {
  n <- 39
  value <- ca_filter_by_water_level(height_eod_cm = numeric(n),
                                    application_type = "ground",
                                    height_threshold_cm = 2)

  expect_vector(value, ptype = logical(), size = n)
})

test_that("ca_filter_by_water_level(): output is correct in simple case 1", {
  height_eod_cm <- c(0, 1, 1.9, 3.2, 3.4, 1.8, 5, 8, 3.2, 1.1)
  expected       <- c(T, T, T  , F  , F  , T  , F, F, F  , T  )

  value <- ca_filter_by_water_level(
    height_eod_cm = height_eod_cm,
    application_type = "ground",
    height_threshold_cm = 2)

  expect_equal(value, expected)
})

test_that("ca_filter_by_water_level(): output is correct in simple case 2", {
  height_eod_cm <- c(0, 1, 1.9, 3.2, 3.4, 1.8, 5, 8, 3.2, 1.1)
  expected       <- !logical(length(height_eod_cm))

  value <- ca_filter_by_water_level(
    height_eod_cm = height_eod_cm,
    application_type = "aerial",
    height_threshold_cm = 2)

  expect_equal(value, expected)
})
