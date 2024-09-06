test_that("ca_to_cluster() returns a numeric of the correct length", {
  n <- 10
  res <- ca_to_cluster(
    date = as.Date("1970-01-01") + 1:n,
    real_height_cm = numeric(n),
    irrigation = logical(n),
    draining = logical(n),
    plan_delay = numeric(n),
    application_day = 5,
    amount = 1,
    application_type = "ground",
    sowing_mmdd = "01-01",
    sowing_yyyy = "1970",
    height_thresh_cm = 1,
    previous_applications = numeric(n)
  )

  expect_vector(res, ptype = double(), size = n)
})
