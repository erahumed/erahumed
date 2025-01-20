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

bench_hbl <- bench::mark(s1 <- run_simulation(s0, "hbl"))
bench_hbc <- bench::mark(s2 <- run_simulation(s1, "hbc"))
bench_ca <- bench::mark(s3 <- run_simulation(s2, "ca"))
bench_ct <- bench::mark(s4 <- run_simulation(s3, "ctc"))

bench <- bind_rows(bench_hbl, bench_hbc, bench_ca, bench_ct) |>
  mutate(layer = c("hbl", "hbc", "ca", "ctc")) |>
  transmute(layer, t = median, t_pct = as.numeric(t / sum(t)))

