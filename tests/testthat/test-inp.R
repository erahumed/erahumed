test_that("compute_inp(): execution succeeds with valid input", {
  model <- erahumed_model()

  expect_no_error( compute_inp(model) )
})

test_that("compute_inp(): no error with simplified outflows_df", {
  cols <- c("date",
            "level",
            "outflow_pujol",
            "outflow_perellonet",
            "is_imputed_level",
            "is_imputed_outflow"
            )
  outflows_df <- albufera_outflows[, cols]

  expect_no_error( compute_inp(erahumed_model(), outflows_df = outflows_df) )
})

test_that("compute_inp(): errors if provided with invalid input data", {
  invalid_outflow_df <- erahumed::albufera_outflows |> dplyr::select(-date)
  invalid_petp_df <- erahumed::albufera_petp |> dplyr::select(-rain_mm)

  expect_error(
    compute_inp(erahumed_model(), outflows_df = invalid_outflow_df),
    class = "compute_inp_argcheck_error"
  )
  expect_error(
    compute_inp(erahumed_model(), petp_df = invalid_petp_df),
    class = "compute_inp_argcheck_error"
  )
})
