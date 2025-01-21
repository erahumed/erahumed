library(erahumed)

date_start <- "2020-01-01"
date_end <- "2020-01-10"
outflows_df <- albufera_outflows |>
  (\(.) .[date_start <= .$date & .$date <= date_end, ])()

sim <- erahumed_simulation() |>
  setup_hydrology(outflows_df = outflows_df)

run_simulation(sim)

debug(erahumed:::compute_hbl)
