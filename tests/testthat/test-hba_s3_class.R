test_that("Ensure constructor raises errors of the correct class", {

  expect_error(make_hba("not a df"),
               class = "make_hba_argcheck_error")

  expect_error(make_hba(data.frame(x = 1:10, y = 10:1)),  # Wrong cols
               class = "make_hba_argcheck_error")

})
