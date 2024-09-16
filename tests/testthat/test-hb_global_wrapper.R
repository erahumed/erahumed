test_that("albufera_hb_global(): no error with default arguments", {
  expect_no_error(albufera_hb_global())
})

test_that("albufera_hb_global(): no error with simplified outflows_df", {
  cols <- c("date",
            "level",
            "outflow_pujol",
            "outflow_perellonet",
            "is_imputed_level",
            "is_imputed_outflow"
            )
  outflows_df <- albufera_outflows[, cols]
  expect_no_error(albufera_hb_global(outflows_df = outflows_df))
})

test_that("albufera_hb_global(): errors if provided with invalid input data", {
  invalid_outflow_df <- erahumed::albufera_outflows |> dplyr::select(-date)
  invalid_petp_df <- erahumed::albufera_petp |> dplyr::select(-rain_mm)
  expect_error(
    albufera_hb_global(outflows_df = invalid_outflow_df),
    class = "albufera_hb_global_datacheck_error"
    )
  expect_error(
    albufera_hb_global(petp_df = invalid_petp_df),
    class = "albufera_hb_global_datacheck_error"
    )
})
