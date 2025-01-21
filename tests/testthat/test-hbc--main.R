test_that("setup_hbc() does not raise an error with valid inputs", {
  simulation <- test_sim_small()
  management_df <- eval( formals(setup_hydrology)$management_df )
  ideal_flow_rate_cm <- eval( formals(setup_hydrology)$ideal_flow_rate_cm )
  height_thresh_cm <- eval( formals(setup_hydrology)$height_thresh_cm )

  expect_no_error( setup_hbc(simulation = simulation,
                             management_df = management_df,
                             ideal_flow_rate_cm = ideal_flow_rate_cm,
                             height_thresh_cm = height_thresh_cm
                             )
                   )
})

test_that("setup_hbc() error if invalid ideal_flow_rate_cm", {
  simulation <- test_sim_small()
  management_df <- eval( formals(setup_hydrology)$management_df )
  ideal_flow_rate_cm <- eval( formals(setup_hydrology)$ideal_flow_rate_cm )
  height_thresh_cm <- eval( formals(setup_hydrology)$height_thresh_cm )

  expect_error(
    setup_hbc(simulation = simulation,
              management_df = management_df,
              ideal_flow_rate_cm = -1,
              height_thresh_cm = height_thresh_cm
    ) ,
    class = "validate_hbc_params_error"
  )

  expect_error(
    setup_hbc(simulation = simulation,
              management_df = management_df,
              ideal_flow_rate_cm = "one",
              height_thresh_cm = height_thresh_cm
              ),
    class = "validate_hbc_params_error"
  )

  expect_error(
    setup_hbc(simulation = simulation,
              management_df = management_df,
              ideal_flow_rate_cm = NA,
              height_thresh_cm = height_thresh_cm
    ),
    class = "validate_hbc_params_error"
  )

  expect_error(
    setup_hbc(simulation = simulation,
              management_df = management_df,
              ideal_flow_rate_cm = Inf,
              height_thresh_cm = height_thresh_cm
              ),
    class = "validate_hbc_params_error"
  )

  expect_error(
    setup_hbc(simulation = simulation,
              management_df = management_df,
              ideal_flow_rate_cm = NaN,
              height_thresh_cm = height_thresh_cm
    ),
    class = "validate_hbc_params_error"
  )

})

test_that("setup_hbc() error if invalid height_thresh_cm", {
  simulation <- test_sim_small()
  management_df <- eval( formals(setup_hydrology)$management_df )
  ideal_flow_rate_cm <- eval( formals(setup_hydrology)$ideal_flow_rate_cm )
  height_thresh_cm <- eval( formals(setup_hydrology)$height_thresh_cm )

  expect_error(
    setup_hbc(simulation = simulation,
              management_df = management_df,
              ideal_flow_rate_cm = ideal_flow_rate_cm,
              height_thresh_cm = -1),
    class = "validate_hbc_params_error"
  )

  expect_error(
    setup_hbc(simulation = simulation,
              management_df = management_df,
              ideal_flow_rate_cm = ideal_flow_rate_cm,
              height_thresh_cm = "two"),
    class = "validate_hbc_params_error"
  )

  expect_error(
    setup_hbc(simulation = simulation,
              management_df = management_df,
              ideal_flow_rate_cm = ideal_flow_rate_cm,
              height_thresh_cm = NA_real_),
    class = "validate_hbc_params_error"
  )

})

test_that("setup_hbc() error for invalid 'management_df'", {
  simulation <- test_sim_small()
  management_df <- erahumed::albufera_management[,-1]  # missing 'date' col
  ideal_flow_rate_cm <- eval( formals(setup_hydrology)$ideal_flow_rate_cm )
  height_thresh_cm <- eval( formals(setup_hydrology)$height_thresh_cm )

  expect_error(setup_hbc(simulation = simulation,
                         management_df = management_df,
                         ideal_flow_rate_cm = ideal_flow_rate_cm,
                         height_thresh_cm = height_thresh_cm),
               class = "validate_hbc_params_error"
               )
})

test_that("setup_hbc() error for invalid 'management_df' date interval", {
  simulation <- test_sim_small()
  management_df <- erahumed::albufera_management[-(18:26),]  # missing days
  ideal_flow_rate_cm <- eval( formals(setup_hydrology)$ideal_flow_rate_cm )
  height_thresh_cm <- eval( formals(setup_hydrology)$height_thresh_cm )

  expect_error(setup_hbc(simulation = simulation,
                         management_df = management_df,
                         ideal_flow_rate_cm = ideal_flow_rate_cm,
                         height_thresh_cm = height_thresh_cm),
               class = "validate_hbc_params_error"
               )
})

test_that("hbc results depend on seed", {
  s1 <- test_sim_small(seed = 1, force = TRUE) |> run_simulation()
  s2 <- test_sim_small(seed = 2, force = TRUE) |> run_simulation()

  expect_false( identical(s1, s2) )
})

test_that("hbc results are reproducible by fixing seed", {
  s1 <- test_sim_small(seed = 1, force = TRUE) |> run_simulation()
  s1bis <- test_sim_small(seed = 1, force = TRUE) |> run_simulation()

  expect_identical(s1, s1bis)
})
