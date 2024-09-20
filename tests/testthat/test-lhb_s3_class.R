test_that("Ensure constructor raises errors of the correct class", {

  expect_error(make_lhb("not a df"),
               class = "make_lhb_argcheck_error")

  expect_error(make_lhb(data.frame(x = 1:10, y = 10:1)),  # Wrong cols
               class = "make_lhb_argcheck_error")

})
