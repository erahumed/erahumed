test_that("ca() execution succeeds with valid input", {
  hbl <- albufera_hb_local(date_min = "2010-01-01", date_max = "2010-01-10")
  expect_no_error(ca(hbl))
})
