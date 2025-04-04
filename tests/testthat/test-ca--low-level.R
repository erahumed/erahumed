test_that("ca_to_cluster() returns a numeric of the correct length", {
  n <- 10
  res <- ca_to_cluster(
    application_day = 5,
    amount_kg = 1,
    seed_day = -4:5,
    plan_delay = numeric(n),
    previous_applications = numeric(n)
  )

  expect_vector(res, ptype = double(), size = n)
})
