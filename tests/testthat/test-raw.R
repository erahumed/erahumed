test_that("Execution succeeds with default input", {
  expect_no_error(raw())
})

test_that("raw(): no error with simplified outflows_df", {
  cols <- c("date",
            "level",
            "outflow_pujol",
            "outflow_perellonet",
            "is_imputed_level",
            "is_imputed_outflow"
  )
  outflows_df <- albufera_outflows[, cols]
  expect_no_error(raw(outflows_df = outflows_df))
})

test_that("raw(): errors if provided with invalid input data", {
  invalid_outflow_df <- erahumed::albufera_outflows |> dplyr::select(-date)
  invalid_petp_df <- erahumed::albufera_petp |> dplyr::select(-rain_mm)
  expect_error(
    raw(outflows_df = invalid_outflow_df),
    class = "raw_argcheck_error"
  )
  expect_error(
    raw(petp_df = invalid_petp_df),
    class = "raw_argcheck_error"
  )
})
