test_that("Constructor succeeds with default arguments", {
  expect_no_error(water_management_scheme())
})

test_that("Assertion succeeds on output of constructor", {
  x <- water_management_scheme()
  expect_no_error(assert_water_management_scheme(x))
})
