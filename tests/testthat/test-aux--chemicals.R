test_that("info_chemicals() succeeds", {
  expect_no_error(info_chemicals())
})

test_that("info_chemicals() returns a list", {
  expect_vector(info_chemicals(), list())
})

test_that("info_chemicals() has names", {
  expect_named(info_chemicals())
})
