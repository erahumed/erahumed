library(erahumed)
library(profvis)
library(dplyr)

date_start <- "2020-01-01"
date_end <- "2020-12-31"

s0 <-
  erahumed_simulation() |>
  setup_inp(outflows_df = albufera_outflows |>
              (\(.) .[date_start <= .$date & .$date <= date_end, ])()
            )

bench_hba <- bench::mark(s1 <- run_simulation(s0, "hba"))
bench_hbp <- bench::mark(s2 <- run_simulation(s1, "hbp"))
bench_ca <- bench::mark(s3 <- run_simulation(s2, "ca"))
bench_ct <- bench::mark(s4 <- run_simulation(s3, "ct"))

bench <- bind_rows(bench_hba, bench_hbp, bench_ca, bench_ct) |>
  mutate(layer = c("hba", "hbp", "ca", "ct")) |>
  transmute(layer, t = median, t_pct = as.numeric(t / sum(t)))

