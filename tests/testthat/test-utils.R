test_that("moving_average() length is correct", {
  x <- 1:10

  expect_length(moving_average(x, 3), length(x))
  expect_length(moving_average(x, 5), length(x))
})

test_that("moving_average() returns the correct result in a basic case", {
  expect_equal(moving_average(1:3, 3), (1 / 3) * c(1 + 1 + 2,
                                                   1 + 2 + 3,
                                                   2 + 3 + 3)
               )
})

test_that("moving_average() raises a warning when 'k' is even", {
  expect_warning(moving_average(1:5, 4))
})

test_that("moving_average() returns the correct result for even 'k'", {
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



test_that("get_mm() and get_dd() returns vectors of same length of input", {
  input <- as.Date(c("2021-01-01", "2019-12-31", "1992-01-24", "2001-10-02"))
  mm <- get_mm(input)
  dd <- get_dd(input)

  expect_length(mm, length(input))
  expect_length(dd, length(input))
  })

test_that("get_mm() and get_dd() returns month and day numbers", {
  input <- as.Date(c("2021-01-01", "2019-12-31", "1992-01-24", "2001-10-02"))
  mm <- get_mm(input)
  dd <- get_dd(input)

  expect_in(mm, 1:12)
  expect_in(dd, 1:31)
})

test_that("get_mm() and get_dd() returns the correct values in simple cases", {
  input <- as.Date(c("2021-01-01", "2019-12-31", "1992-01-24", "2001-10-02"))
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

test_that("unit conversion helpers return correct values in simple cases", {
  input_m3_s <- 1  # 1 m3/s of water during 1 day
  area_m2 <- 1

  expected <- 100 * s_per_day()
  expect_equal(m3_s_to_cm_day(input_m3_s, area_m2), expected)
})
