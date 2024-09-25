test_that("plot.erahumed_ca does not produce an error with valid inputs", {
  outflows_df <- albufera_outflows |>
    dplyr::filter("2010-01-01" <= date, date <= "2010-01-10")

  ca_res <- erahumed_model() |>
    compute_inp(outflows_df = outflows_df) |>
    compute_hba() |>
    compute_hbp() |>
    compute_ca() |>
    ca()

  cluster_id <- ca_res$output$cluster_id[1]

  expect_no_error(plot(ca_res, cluster_id = cluster_id))
})
