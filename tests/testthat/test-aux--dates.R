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
