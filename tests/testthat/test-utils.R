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
