test_that("Constructor succeeds with default arguments", {
  expect_no_error(wms())
})

test_that("Assertion succeeds on output of constructor", {
  x <- wms()
  expect_no_error(assert_wms(x))
})
