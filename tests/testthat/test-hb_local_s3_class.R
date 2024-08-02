test_that("Ensure constructor raises errors of the correct class", {

  expect_error(make_hb_local("not a df"),
               class = "make_hb_local_argcheck_error")

  expect_error(make_hb_local(data.frame(x = 1:10, y = 10:1)),  # Wrong cols
               class = "make_hb_local_argcheck_error")

})
