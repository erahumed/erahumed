test_that("Ensure constructor raises errors of the correct class", {

  expect_error(make_raw("not a df"),
               class = "make_raw_argcheck_error")

  skip("Column check not implemented for 'erahumed_raw' S3 constructor.")
  expect_error(make_raw(data.frame(x = 1:10, y = 10:1)),  # Wrong cols
               class = "make_raw_argcheck_error")

})
