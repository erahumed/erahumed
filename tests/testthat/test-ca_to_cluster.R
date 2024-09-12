test_that("ca_to_cluster() returns a numeric of the correct length", {
  n <- 10
  res <- ca_to_cluster(
    real_height_cm = numeric(n),
    seed_day = -4:5,
    real_irrigation = logical(n),
    real_draining = logical(n),
    plan_delay = numeric(n),
    application_day = 5,
    amount = 1,
    application_type = "ground",
    height_thresh_cm = 1,
    previous_applications = numeric(n)
  )

  expect_vector(res, ptype = double(), size = n)
})
