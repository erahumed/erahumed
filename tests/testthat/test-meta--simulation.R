expect_input_error <- function(object) {
  expect_error(object, class = "erahumed_input_error")
}

test_that("Error if invalid date parameters", {
  precipitation_mm <- NULL

  invalid_date_start <- NA
  invalid_date_end <- "2020"

  expect_input_error( erahumed_simulation(date_start = invalid_date_start) )
  expect_input_error( erahumed_simulation(date_end = invalid_date_end) )
})

test_that("Error if 'date_end' precedes 'date_start'", {
  invalid_date_window <- c("2020-01-01", "2019-01-01")

  expect_input_error( erahumed_simulation(date_start = invalid_date_window[1],
                                          date_end = invalid_date_window[2]
                                          ) )
})

test_that("Error if invalid input data", {
  precipitation_mm <- NULL

  invalid_outflows_df <- erahumed::albufera_outflows |> dplyr::select(-date)
  invalid_weather_df <- erahumed::albufera_weather |>
    dplyr::select(-precipitation_mm)

  expect_input_error( erahumed_simulation(outflows_df = invalid_outflows_df) )
  expect_input_error( erahumed_simulation(weather_df = invalid_weather_df) )
})

test_that("Error if 'date' cols of input dfs are not intervals", {
  invalid_outflows_df <- erahumed::albufera_outflows[-10, ]
  invalid_weather_df <- erahumed::albufera_weather[-10, ]

  expect_input_error( erahumed_simulation(outflows_df = invalid_outflows_df) )
  expect_input_error( erahumed_simulation(weather_df = invalid_weather_df) )
})

test_that("Error if no data for provided date interval", {
  precipitation_mm <- NULL

  empty_window <- c("1800-01-01", "1800-12-31")

  expect_input_error( erahumed_simulation(date_start = empty_window[1],
                                          date_end = empty_window[2])
                      )
})

test_that("Error if invalid variety proportions", {
  expect_input_error( erahumed_simulation(variety_prop = c(-6,2,2)) )
  expect_input_error( erahumed_simulation(variety_prop = c(5,5)) )
})

test_that("Error if invalid ditch_level_m", {
  expect_input_error( erahumed_simulation(ditch_level_m = -1) )
  expect_input_error( erahumed_simulation(ditch_level_m = "one") )
  expect_input_error( erahumed_simulation(ditch_level_m = NA) )
  expect_input_error( erahumed_simulation(ditch_level_m = Inf) )
  expect_input_error( erahumed_simulation(ditch_level_m = NaN) )
})


test_that("Error if invalid ideal_flow_rate_cm", {
  expect_input_error( erahumed_simulation(ideal_flow_rate_cm = -1) )
  expect_input_error( erahumed_simulation(ideal_flow_rate_cm = "one") )
  expect_input_error( erahumed_simulation(ideal_flow_rate_cm = NA) )
  expect_input_error( erahumed_simulation(ideal_flow_rate_cm = Inf) )
  expect_input_error( erahumed_simulation(ideal_flow_rate_cm = NaN) )
})

test_that("Error if invalid height_thresh_cm", {
  expect_input_error( erahumed_simulation(height_thresh_cm = -1) )
  expect_input_error( erahumed_simulation(height_thresh_cm = "two") )
  expect_input_error( erahumed_simulation(height_thresh_cm = NA_real_) )
})

test_that("Error if invalid 'management_df'", {
  invalid_mgmt_df <- erahumed::albufera_management[,-1]  # missing 'date'

  expect_input_error( erahumed_simulation(management_df = invalid_mgmt_df) )
})

test_that("Error if invalid 'management_df' date interval", {
  invalid_mgmt_df <- erahumed::albufera_management[-(18:26),]  # missing days

  expect_input_error( erahumed_simulation(management_df = invalid_mgmt_df) )
})









test_that("Simulation results depend on seed", {
  s1 <- test_sim_small(seed = 1, force = TRUE)
  s2 <- test_sim_small(seed = 2, force = TRUE)

  expect_false( identical(s1, s2) )
})

test_that("Simulation results are reproducible by fixing seed", {
  s1 <- test_sim_small(seed = 1, force = TRUE)
  s2 <- test_sim_small(seed = 1, force = TRUE)

  expect_identical(s1, s2)
})


skip("Temporarily Skipped")

test_that("Constructor succeeds", {
  expect_no_error( erahumed_simulation() )
})

test_that("Validator returns TRUE on constructor output", {
  m <- erahumed_simulation()
  expect_true( is_erahumed_simulation(m) )
})

test_that("Validator: FALSE if object is not a list", {
  obj <- structure("Not a list", class = class(erahumed_simulation()))
  expect_false( is_erahumed_simulation(obj) )
})

test_that("Validator: FALSE if list elements are not of the right S3 class", {
  obj <- erahumed_simulation()
  obj[["invalid_element"]] <- list()  # Not of 'simulation_layer' class!
  expect_false( is_erahumed_simulation(obj) )
})

test_that("Validator: FALSE if object is not of the right S3 class", {
  expect_false( is_erahumed_simulation(list()) )
})

test_that("print() method succeeds", {
  obj <- erahumed_simulation()
  expect_no_error(capture.output(print(obj)))
})

test_that("print() method returns invisibly", {
  obj <- erahumed_simulation()
  print_res <- expect_output(print(obj))
  expect_identical(print_res, obj)
})

test_that("summary() method succeeds", {
  expect_no_error( capture.output(summary(erahumed_simulation())) )
})
