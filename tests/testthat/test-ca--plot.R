test_that("plot.erahumed_ca does not produce an error with valid inputs", {
  outflows_df <- albufera_outflows |>
    dplyr::filter("2010-01-01" <= date, date <= "2010-01-10")

  m <- erahumed_model() |>
    compute_inp(outflows_df = outflows_df) |>
    compute_hba() |>
    compute_hbp() |>
    compute_ca()

  cluster_id <- component_output(m, "ca")$cluster_id[1]

  expect_no_error(plot(ca(m), cluster_id = cluster_id))
})
