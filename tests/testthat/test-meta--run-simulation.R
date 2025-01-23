test_that("run_simulation() succeeds with valid simulation input", {
  outflows_df <- albufera_outflows |>
    dplyr::filter("2010-01-01" <= date, date <= "2010-01-10")

  s <- test_sim_small() |>
    setup_hydrology(outflows_df = outflows_df)  # drop simulation outputs

  expect_no_error(run_simulation(s))
})
