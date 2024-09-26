test_that("compute_hba(): no error with valid arguments", {
  expect_no_error( compute_hba(test_objects$mod_small) )
})

test_that("compute_hba(): returns an object of the correct class", {
  obj <- compute_hba(test_objects$mod_small)
  expect_s3_class(obj, class(erahumed_model()))
})
