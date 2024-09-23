test_that("Ensure constructor raises errors of the correct class", {

  expect_error(make_inp("not a df"),
               class = "make_inp_argcheck_error")

  skip("Column check not implemented for 'erahumed_raw' S3 constructor.")
  expect_error(make_inp(data.frame(x = 1:10, y = 10:1)),  # Wrong cols
               class = "make_inp_argcheck_error")

})
