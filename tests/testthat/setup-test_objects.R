set.seed(840)

test_objects <- list()

test_objects$mod_small <- erahumed_model() |>
  compute_inp(
    outflows_df = albufera_outflows |>
      dplyr::filter("2010-01-01" <= date, date < "2010-01-10")
  ) |>
  compute_hba() |>
  compute_hbp() |>
  compute_ca()

test_objects$mod_med <- erahumed_model() |>
  compute_inp(
    outflows_df = albufera_outflows |>
      dplyr::filter("2010-04-01" <= date, date < "2010-06-01")
  ) |>
  compute_hba() |>
  compute_hbp() |>
  compute_ca()

test_objects$mod_large <- erahumed_model() |>
  compute_inp(
    outflows_df = albufera_outflows |>
      dplyr::filter("2010-01-01" <= date, date < "2012-01-01")
  ) |>
  compute_hba() |>
  compute_hbp() |>
  compute_ca()
