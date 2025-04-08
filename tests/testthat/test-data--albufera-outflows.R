test_that("data.frame has no NAs", {
  expect_no_error(na.fail(albufera_outflows))
})

test_that("date domain is an interval", {
  expect_true(all( diff(albufera_outflows$date) == 1) )
})

