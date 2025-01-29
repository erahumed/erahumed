library(erahumed)
library(profvis)


date_start <- "2020-01-01"
date_end <- "2020-12-31"
outflows_df <- albufera_outflows |>
  (\(.) .[date_start <= .$date & .$date <= date_end, ])()

sim <- erahumed_simulation() |>
  setup_hydrology(outflows_df = outflows_df)

profvis::profvis(sim <- run_simulation(sim))
