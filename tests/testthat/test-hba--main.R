test_that("compute_hba(): no error with valid arguments", {
  m <- test_mod_small()

  expect_no_error( compute_hba(m) )
})

test_that("compute_hba(): returns an object of the correct class", {
  obj <- compute_hba(test_mod_small())
  expect_s3_class(obj, class(erahumed_simulation()))
})
