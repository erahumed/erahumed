test_that("plot.erahumed_ct does not produce an error with valid inputs", {
  ct_res <- ct(test_mod_small())
  expect_no_error( plot(ct_res) )
})

