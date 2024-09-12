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



test_that("get_mm() and get_dd() outputs have same length of input", {
  input <- as.POSIXlt(c("2021-01-01", "2019-12-31", "1992-01-24", "2001-10-02"))
  mm <- get_mm(input)
  dd <- get_dd(input)

  expect_length(mm, length(input))
  expect_length(dd, length(input))
  })

test_that("get_mm() and get_dd() return month and day numbers", {
  input <- as.POSIXlt(c("2021-01-01", "2019-12-31", "1992-01-24", "2001-10-02"))
  mm <- get_mm(input)
  dd <- get_dd(input)

  expect_in(mm, 1:12)
  expect_in(dd, 1:31)
})

test_that("get_mm() and get_dd(): correct values in simple cases", {
  input <- as.POSIXlt(c("2021-01-01", "2019-12-31", "1992-01-24", "2001-10-02"))
  mm <- get_mm(input)
  dd <- get_dd(input)

  expect_equal(mm, c(1, 12, 1, 10))
  expect_equal(dd, c(1, 31, 24, 2))
})



test_that("cm_day_to_m3_s() returns correct value in simple case", {
  input_cm <- 1 * 100  # 1 m3 of water in 1 day
  area_m2 <- 1

  expected <- 1 / s_per_day()
  expect_equal(cm_day_to_m3_s(input_cm, area_m2), expected)
})

test_that("m3_s_to_cm_day() returns correct value in simple case", {
  input_m3_s <- 1  # 1 m3/s of water during 1 day
  area_m2 <- 1

  expected <- 100 * s_per_day()
  expect_equal(m3_s_to_cm_day(input_m3_s, area_m2), expected)
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

