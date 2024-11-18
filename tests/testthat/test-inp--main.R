test_that("setup_inp(): execution succeeds with valid input", {
  simulation <- erahumed_simulation()

  expect_no_error( setup_inp(simulation) )
})

test_that("setup_inp(): no error with simplified outflows_df", {
  cols <- c("date",
            "level",
            "outflow_pujol",
            "outflow_perellonet",
            "is_imputed_level",
            "is_imputed_outflow"
            )
  outflows_df <- albufera_outflows[, cols]

  expect_no_error( setup_inp(erahumed_simulation(), outflows_df = outflows_df) )
})

test_that("setup_inp(): errors if provided with invalid input data", {
  invalid_outflow_df <- erahumed::albufera_outflows |> dplyr::select(-date)
  invalid_weather_df <- erahumed::albufera_weather |> dplyr::select(-precipitation_mm)

  expect_error(
    setup_inp(erahumed_simulation(), outflows_df = invalid_outflow_df),
    class = "validate_inp_params_error"
  )
  expect_error(
    setup_inp(erahumed_simulation(), weather_df = invalid_weather_df),
    class = "validate_inp_params_error"
  )
})

test_that("setup_inp(): error if 'date' cols of input dfs are not intervals", {

  # Arbitrarily remove one row (one day)
  invalid_weather_df <- erahumed::albufera_weather[-10, ]
  invalid_outflow_df <- erahumed::albufera_outflows[-10, ]

  expect_error(
    setup_inp(erahumed_simulation(), outflows_df = invalid_outflow_df),
    class = "validate_inp_params_error"
  )
  expect_error(
    setup_inp(erahumed_simulation(), weather_df = invalid_weather_df),
    class = "validate_inp_params_error"
  )
})
