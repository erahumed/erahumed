test_that("setup_hba(): no error with valid arguments", {
  m <- test_sim_small()

  expect_no_error( setup_hba(m) )
})

test_that("setup_hba(): returns an object of the correct class", {
  obj <- setup_hba(test_sim_small())
  expect_s3_class(obj, class(erahumed_simulation()))
})
