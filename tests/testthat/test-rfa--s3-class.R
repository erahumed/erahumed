test_that("new_rfa() constructor succeeds", {
  expect_no_error(new_rfa())
})

test_that("new_rfa() creates an object of the correct class", {
  x <- new_rfa()
  expect_s3_class(x, "erahumed_rfa")
})

test_that("Validator succeeds", {
  x <- new_rfa()
  expect_no_error(assert_rfa(x))
})

