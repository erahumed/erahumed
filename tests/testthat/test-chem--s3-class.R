test_that("chemical() constructor succeeds (using thin wrapper)", {
  expect_no_error(acetamiprid())
})

test_that("chemical() constructor returns an object of the correct S3 class", {
  expect_s3_class(acetamiprid(), class = "erahumed_chemical")
})
