test_that("albufera_hb_global(): no error with default arguments", {
  expect_no_error(albufera_hb_global())
})

test_that("albufera_hb_global(): errors if provided with invalid input data", {
  invalid_outflow_df <- erahumed::albufera_outflows |> dplyr::select(-date)
  invalid_weather_df <- erahumed::albufera_weather |> dplyr::select(-P)
  expect_error(albufera_hb_global(outflows_df = invalid_outflow_df))
  expect_error(albufera_hb_global(weather_df = invalid_weather_df))
})



# TODO: These tests are really tests on the lower level logical units... move?
test_that("Hydrological balance checks out", {
  tol <- 1e-12

  df <- albufera_hb_global()

  res <- df |>
    dplyr::mutate(
      x = volume_change,
      y = (inflow_total - outflow_total) * s_per_day() + volume_change_petp
    ) |>
    dplyr::filter(abs(x - y) / mean(abs(x)) > tol)

  expect_equal(nrow(res), 0)
  })

test_that("outflow_extra != requires inflow_total == 0", {
  tol <- 1e-12

  df <- albufera_hb_global()

  res <- df |>
    dplyr::filter(abs(inflow_total) > tol * mean(abs(inflow_total)),
                  abs(outflow_extra) > tol * mean(abs(outflow_total))
                  )

  expect_equal(nrow(res), 0)
})

