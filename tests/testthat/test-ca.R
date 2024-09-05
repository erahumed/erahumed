test_that("ca_to_cluster() returns a numeric of the correct length", {
  n <- 10
  res <- ca_to_cluster(
    date = as.Date("1970-01-01") + 1:10,
    real_height_cm = numeric(n),
    irrigation = logical(n),
    draining = logical(n),
    plan_delay = numeric(n),
    application_day = 5,
    amount = 1,
    sowing_day = "01-01",
    height_thresh_cm = 1,
    previous_applications = numeric(n)
    )

  expect_vector(res, ptype = double(), size = n)
})

test_that("ca() execution succeeds with valid input", {
  hbl <- albufera_hb_local(date_min = "2010-01-01", date_max = "2010-01-10")
  expect_no_error(ca(hbl))
})
