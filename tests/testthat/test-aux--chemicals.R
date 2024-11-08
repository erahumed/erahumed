test_that("chemicals() succeeds", {
  expect_no_error(chemicals())
})

test_that("chemicals() returns a character vector", {
  expect_vector(chemicals(), character())
})

test_that("chemical_info() succeeds with valid input", {
  name <- chemicals()[[1]]
  expect_no_error(chemical_properties(name))
})

test_that("chemical_info() returns a list", {
  name <- chemicals()[[1]]
  expect_vector(chemical_properties(name), list())
})
