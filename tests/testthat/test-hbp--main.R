test_that("setup_hbp() does not raise an error with valid inputs", {
  expect_no_error( setup_hbp(test_sim_small()) )
})

test_that("setup_hbp() error if invalid ideal_flow_rate_cm", {
  expect_error(
    setup_hbp(test_sim_small(), ideal_flow_rate_cm = -1),
    class = "validate_hbp_params_error"
  )

  expect_error(
    setup_hbp(test_sim_small(), ideal_flow_rate_cm = "one"),
    class = "validate_hbp_params_error"
  )

  expect_error(
    setup_hbp(test_sim_small(), ideal_flow_rate_cm = NA),
    class = "validate_hbp_params_error"
  )

  expect_error(
    setup_hbp(test_sim_small(), ideal_flow_rate_cm = Inf),
    class = "validate_hbp_params_error"
  )

  expect_error(
    setup_hbp(test_sim_small(), ideal_flow_rate_cm = NaN),
    class = "validate_hbp_params_error"
  )

})

test_that("setup_hbp() error if invalid height_thresh_cm", {
  expect_error(
    setup_hbp(test_sim_small(), height_thresh_cm = -1),
    class = "validate_hbp_params_error"
  )

  expect_error(
    setup_hbp(test_sim_small(), height_thresh_cm = "two"),
    class = "validate_hbp_params_error"
  )

  expect_error(
    setup_hbp(test_sim_small(), height_thresh_cm = NA_real_),
    class = "validate_hbp_params_error"
  )

})

test_that("setup_hbp() error if invalid input data-frames", {
  expect_error(
    setup_hbp(test_sim_small(),
              management_df = erahumed::albufera_management[,-1]
              ),
    class = "validate_hbp_params_error"
    )

  expect_error(
    setup_hbp(test_sim_small(), clusters_df = erahumed::albufera_clusters[,-1]),
    class = "validate_hbp_params_error"
    )

})

test_that("HBP results depend on seed", {
  s1 <- setup_hbp(test_sim_small(), seed = 1) |> run_simulation("hbp")
  s2 <- setup_hbp(test_sim_small(), seed = 2) |> run_simulation("hbp")

  expect_false( identical(s1, s2) )
})

test_that("HBP results are reproducible by fixing seed", {
  s1 <- setup_hbp(test_sim_small(), seed = 1) |> run_simulation("hbp")
  s2 <- setup_hbp(test_sim_small(), seed = 1) |> run_simulation("hbp")

  expect_identical(s1, s2)
})
