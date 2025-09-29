test_that("chemical() constructor succeeds (using thin wrapper)", {
  expect_no_error(acetamiprid())
})

test_that("chemical() constructor returns an object of the correct S3 class", {
  expect_s3_class(acetamiprid(), class = "erahumed_chemical")
})

test_that("print() method succeeds", {
  obj <- acetamiprid()
  expect_no_error( print(obj) ) |>
    capture.output()
})

test_that("summary() method succeeds", {
  obj <- acetamiprid()
  expect_no_error( summary(obj) ) |>
    capture.output()
})
