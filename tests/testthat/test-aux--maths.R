test_that("moving_average() length is correct", {
  x <- 1:10

  expect_length(moving_average(x, 3), length(x))
  expect_length(moving_average(x, 5), length(x))
})

test_that("moving_average() gives the correct result in a basic case", {
  expect_equal(moving_average(1:3, 3), (1 / 3) * c(1 + 1 + 2,
                                                   1 + 2 + 3,
                                                   2 + 3 + 3)
  )
})

test_that("moving_average() raises a warning when 'k' is even", {
  expect_warning(moving_average(1:5, 4))
})

test_that("moving_average() gives the correct result for even 'k'", {
  res <- suppressWarnings(moving_average(1:5, 4))
  expect_equal(
    res,
    (1 / 5) * c(1 + 1 + 1 + 2 + 3,
                1 + 1 + 2 + 3 + 4,
                1 + 2 + 3 + 4 + 5,
                2 + 3 + 4 + 5 + 5,
                3 + 4 + 5 + 5 + 5)
  )
})

test_that("lgl_buffer() output has the correct type and length", {
  n <- 23
  input <- numeric(n)
  input[c(1, 5, 9, 19)] <- c(3, 2.1, 7, -12)

  expect_vector(lgl_buffer(input), ptype = logical(), size = n)
})

test_that("lgl_buffer() output is correct in simple case 1", {

  distance <- 0
  x        <- c(0.0, 0.0, 0.1, 0.0, 1.3, 0.0, 1.8, 0.1, 0.0, 0.2)
  expected <- c(  F,   F,   T,   F,   T,   F,   T,   T,   F,   T)

  expect_equal(lgl_buffer(x, distance), expected)
})

test_that("lgl_buffer() output is correct in simple case 2", {

  distance <- 1
  x        <- c(0.0, 0.0, 0.1, 0.0, 1.3, 0.0, 1.8, 0.1, 0.0, 0.2)
  expected <- c(  F,   T,   T,   T,   T,   T,   T,   T,   T,   T)

  expect_equal(lgl_buffer(x, distance), expected)
})

test_that("lgl_buffer() output is correct in simple case 3", {

  distance <- 1
  x        <- c(0.0, 0.0, 0.0, 0.1, 0.0, 1.3, 0.0, 0.0, 0.0, 0.0, 0.1, 0.0, 0.0)
  expected <- c(  F,   F,   T,   T,   T,   T,   T,   F,   F,   T,   T,   T,   F)

  expect_equal(lgl_buffer(x, distance), expected)
})

test_that("pmin2() outputs are correct in simple cases", {
  expect_equal(
    pmin2(c(0.1, -1, 2, 3, -0.7, -0.1, 2, 3), thresh = 0),
    c(0, -1, 0, 0, -0.7, -0.1, 0, 0)
    )

  expect_equal(
    pmin2(c(2, 0.5, 1, 1.1, -1, 0.9, 1.4, 3), thresh = 1),
    c(1, 0.5, 1, 1, -1, 0.9, 1, 1)
  )
})

test_that("pmax2() outputs are correct in simple cases", {
  expect_equal(
    pmax2(c(0.1, -1, 2, 3, -0.7, -0.1, 2, 3), thresh = 0),
    c(0.1, 0, 2, 3, 0, 0, 2, 3)
  )

  expect_equal(
    pmax2(c(2, 0.5, 1, 1.1, -1, 0.9, 1.4, 3), thresh = 1),
    c(2, 1, 1, 1.1, 1, 1, 1.4, 3)
  )
})
