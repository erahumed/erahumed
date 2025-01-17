test_that("setup_hbp() does not raise an error with valid inputs", {
  simulation <- test_sim_small()
  management_df <- eval( formals(setup_hydrology)$management_df )
  ideal_flow_rate_cm <- eval( formals(setup_hydrology)$ideal_flow_rate_cm )
  height_thresh_cm <- eval( formals(setup_hydrology)$height_thresh_cm )

  expect_no_error( setup_hbp(simulation = simulation,
                             management_df = management_df,
                             ideal_flow_rate_cm = ideal_flow_rate_cm,
                             height_thresh_cm = height_thresh_cm
                             )
                   )
})

test_that("setup_hbp() error if invalid ideal_flow_rate_cm", {
  simulation <- test_sim_small()
  management_df <- eval( formals(setup_hydrology)$management_df )
  ideal_flow_rate_cm <- eval( formals(setup_hydrology)$ideal_flow_rate_cm )
  height_thresh_cm <- eval( formals(setup_hydrology)$height_thresh_cm )

  expect_error(
    setup_hbp(simulation = simulation,
              management_df = management_df,
              ideal_flow_rate_cm = -1,
              height_thresh_cm = height_thresh_cm
    ) ,
    class = "validate_hbp_params_error"
  )

  expect_error(
    setup_hbp(simulation = simulation,
              management_df = management_df,
              ideal_flow_rate_cm = "one",
              height_thresh_cm = height_thresh_cm
              ),
    class = "validate_hbp_params_error"
  )

  expect_error(
    setup_hbp(simulation = simulation,
              management_df = management_df,
              ideal_flow_rate_cm = NA,
              height_thresh_cm = height_thresh_cm
    ),
    class = "validate_hbp_params_error"
  )

  expect_error(
    setup_hbp(simulation = simulation,
              management_df = management_df,
              ideal_flow_rate_cm = Inf,
              height_thresh_cm = height_thresh_cm
              ),
    class = "validate_hbp_params_error"
  )

  expect_error(
    setup_hbp(simulation = simulation,
              management_df = management_df,
              ideal_flow_rate_cm = NaN,
              height_thresh_cm = height_thresh_cm
    ),
    class = "validate_hbp_params_error"
  )

})

test_that("setup_hbp() error if invalid height_thresh_cm", {
  simulation <- test_sim_small()
  management_df <- eval( formals(setup_hydrology)$management_df )
  ideal_flow_rate_cm <- eval( formals(setup_hydrology)$ideal_flow_rate_cm )
  height_thresh_cm <- eval( formals(setup_hydrology)$height_thresh_cm )

  expect_error(
    setup_hbp(simulation = simulation,
              management_df = management_df,
              ideal_flow_rate_cm = ideal_flow_rate_cm,
              height_thresh_cm = -1),
    class = "validate_hbp_params_error"
  )

  expect_error(
    setup_hbp(simulation = simulation,
              management_df = management_df,
              ideal_flow_rate_cm = ideal_flow_rate_cm,
              height_thresh_cm = "two"),
    class = "validate_hbp_params_error"
  )

  expect_error(
    setup_hbp(simulation = simulation,
              management_df = management_df,
              ideal_flow_rate_cm = ideal_flow_rate_cm,
              height_thresh_cm = NA_real_),
    class = "validate_hbp_params_error"
  )

})

test_that("setup_hbp() error for invalid 'management_df'", {
  simulation <- test_sim_small()
  management_df <- erahumed::albufera_management[,-1]  # missing 'date' col
  ideal_flow_rate_cm <- eval( formals(setup_hydrology)$ideal_flow_rate_cm )
  height_thresh_cm <- eval( formals(setup_hydrology)$height_thresh_cm )

  expect_error(setup_hbp(simulation = simulation,
                         management_df = management_df,
                         ideal_flow_rate_cm = ideal_flow_rate_cm,
                         height_thresh_cm = height_thresh_cm),
               class = "validate_hbp_params_error"
               )
})

test_that("setup_hbp() error for invalid 'management_df' date interval", {
  simulation <- test_sim_small()
  management_df <- erahumed::albufera_management[-(18:26),]  # missing days
  ideal_flow_rate_cm <- eval( formals(setup_hydrology)$ideal_flow_rate_cm )
  height_thresh_cm <- eval( formals(setup_hydrology)$height_thresh_cm )

  expect_error(setup_hbp(simulation = simulation,
                         management_df = management_df,
                         ideal_flow_rate_cm = ideal_flow_rate_cm,
                         height_thresh_cm = height_thresh_cm),
               class = "validate_hbp_params_error"
               )
})

test_that("HBP results depend on seed", {
  s1 <- test_sim_small(seed = 1, force = TRUE) |> run_simulation("hbp")
  s2 <- test_sim_small(seed = 2, force = TRUE) |> run_simulation("hbp")

  expect_false( identical(s1, s2) )
})

test_that("HBP results are reproducible by fixing seed", {
  s1 <- test_sim_small(seed = 1, force = TRUE) |> run_simulation("hbp")
  s1bis <- test_sim_small(seed = 1, force = TRUE) |> run_simulation("hbp")

  expect_identical(s1, s1bis)
})
