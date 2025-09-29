test_that("new_rfms_map() constructor succeeds", {
  expect_no_error(new_rfms_map())
})

test_that("new_rfms_map() creates an object of the correct class", {
  x <- new_rfms_map()
  expect_s3_class(x, "erahumed_rfms_map")
})

test_that("Validator succeeds", {
  x <- new_rfms_map()
  expect_no_error(assert_rfms_map(x))
})

test_that("print() method succeeds", {
  x <- new_rfms_map()
  expect_no_error(print(x)) |>
    capture.output()
})

test_that("summary() method succeeds", {
  x <- new_rfms_map()
  expect_no_error(summary(x)) |>
    capture.output()
})
