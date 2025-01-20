test_that("Execution succeeds with valid input", {
  expect_no_error( setup_ca(test_sim_small(),
                            ca_schedules_df = erahumed::albufera_ca_schedules)
                   )
})

