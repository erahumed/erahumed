test_that("setup_inp(): execution succeeds with valid input", {

  simulation <- erahumed_simulation()
  outflows_df <- eval(formals(setup_hydrology)$outflows_df)
  weather_df <- eval(formals(setup_hydrology)$weather_df)
  variety_prop <- eval(formals(setup_hydrology)$variety_prop)
  seed <- eval(formals(setup_hydrology)$seed)


  expect_no_error( setup_inp(simulation = simulation,
                             outflows_df = outflows_df,
                             weather_df = weather_df,
                             variety_prop = variety_prop,
                             seed = seed)
                   )
})

test_that("setup_inp(): no error with simplified outflows_df", {
  simulation <- erahumed_simulation()
  outflows_df <- eval(formals(setup_hydrology)$outflows_df)
  weather_df <- eval(formals(setup_hydrology)$weather_df)
  variety_prop <- eval(formals(setup_hydrology)$variety_prop)
  seed <- eval(formals(setup_hydrology)$seed)

  cols <- c("date",
            "level",
            "outflow_pujol",
            "outflow_perellonet",
            "is_imputed_level",
            "is_imputed_outflow"
            )
  outflows_df <- albufera_outflows[, cols]

  expect_no_error( setup_inp(simulation = simulation,
                             outflows_df = outflows_df,
                             weather_df = weather_df,
                             variety_prop = variety_prop,
                             seed = seed)
  )
})

test_that("setup_inp(): errors if provided with invalid input data", {
  simulation <- erahumed_simulation()
  outflows_df <- eval(formals(setup_hydrology)$outflows_df)
  weather_df <- eval(formals(setup_hydrology)$weather_df)
  variety_prop <- eval(formals(setup_hydrology)$variety_prop)
  seed <- eval(formals(setup_hydrology)$seed)

  expect_error(
    setup_inp(
      simulation = simulation,
      outflows_df = erahumed::albufera_outflows |> dplyr::select(-date),
      weather_df = weather_df,
      variety_prop = variety_prop,
      seed = seed
      ),
    class = "validate_inp_params_error")

  expect_error(
    setup_inp(
      simulation = simulation,
      outflows_df = outflows_df,
      weather_df = erahumed::albufera_weather |> dplyr::select(-precipitation_mm),
      variety_prop = variety_prop,
      seed = seed
    ),
    class = "validate_inp_params_error")

})

test_that("setup_inp(): error if 'date' cols of input dfs are not intervals", {
  simulation <- erahumed_simulation()
  outflows_df <- eval(formals(setup_hydrology)$outflows_df)
  weather_df <- eval(formals(setup_hydrology)$weather_df)
  variety_prop <- eval(formals(setup_hydrology)$variety_prop)
  seed <- eval(formals(setup_hydrology)$seed)

  expect_error(
    setup_inp(
      simulation = simulation,
      outflows_df = erahumed::albufera_outflows[-10, ],
      weather_df = weather_df,
      variety_prop = variety_prop,
      seed = seed
    ),
    class = "validate_inp_params_error")

  expect_error(
    setup_inp(
      simulation = simulation,
      outflows_df = outflows_df,
      weather_df = erahumed::albufera_weather[-10, ],
      variety_prop = variety_prop,
      seed = seed
    ),
    class = "validate_inp_params_error")
})

test_that("setup_inp(): no error with modified variety proportions", {
  simulation <- erahumed_simulation()
  outflows_df <- eval(formals(setup_hydrology)$outflows_df)
  weather_df <- eval(formals(setup_hydrology)$weather_df)
  variety_prop <- c(6,2,2)
  seed <- eval(formals(setup_hydrology)$seed)


  expect_no_error( setup_inp(simulation = simulation,
                             outflows_df = outflows_df,
                             weather_df = weather_df,
                             variety_prop = variety_prop,
                             seed = seed)
  )
})

test_that("setup_inp(): error with invalid variety proportions", {
  simulation <- erahumed_simulation()
  outflows_df <- eval(formals(setup_hydrology)$outflows_df)
  weather_df <- eval(formals(setup_hydrology)$weather_df)
  seed <- eval(formals(setup_hydrology)$seed)

  expect_error(
    setup_inp(
      simulation = simulation,
      outflows_df = outflows_df,
      weather_df = weather_df,
      variety_prop = c(-6,2,2),
      seed = seed
    ),
    class = "validate_inp_params_error")

  expect_error(
    setup_inp(
      simulation = simulation,
      outflows_df = outflows_df,
      weather_df = weather_df,
      variety_prop = c(5,5),
      seed = seed
    ),
    class = "validate_inp_params_error")
})
