library(erahumed)
library(dplyr)
library(ggplot2)

date_start <- "2013-01-01"
date_end <- "2013-12-31"
outflows_df <- albufera_outflows |>
  (\(.) .[date_start <= .$date & .$date <= date_end, ])()

sim <- erahumed_simulation() |>
  setup_hydrology(outflows_df = outflows_df)

sim <- run_simulation(sim)



# profvis::profvis(run_simulation(sim))

sim$rl$output$mspaf_acute |> max(na.rm = T)

neg_log_surv <- function(p) {
  -log(1-p)
}

neg_log_surv_inv <- function(l) {
  1-exp(-l)
}

sim$ctl$output |>
  filter("2013-04-20" < date, date < "2013-10-30") |>
  filter(chemical == "Azoxy") |>
  summarise(cw = max(cw, na.rm = T), .by = element_id)

sim$ctl$output |>
  filter("2013-04-20" < date, date < "2013-10-30") |>
  filter(chemical == "Azoxy") |>
  #filter(1e6 * cw > 1e-10) |>
  summarise(cw = max(cw, na.rm = T), .by = element_id) |>
  #filter(1e6 * cw > 1e-15) |>
  ggplot(aes(y = 1e6 * cw, x = 0)) +
  geom_boxplot() + geom_jitter()

# plotly::ggplotly()
#
#

max(sim$rl$output$paf_chronic, na.rm = TRUE)

mutate(sim$rc$output, element_type = "clusters") |>
  union(mutate(sim$rd$output, element_type = "ditches")) |>
  union(mutate(sim$rl$output, element_type = "lake")) |>
  filter("2013-04-20" < date, date < "2013-11-30") |>
  summarise(paf = 1 - prod(1-paf_chronic), .by = c(element_id, date, element_type)) |>
  summarise(paf_max = max(paf, na.rm = TRUE), .by = c(element_id, element_type)) |>
  ggplot(aes(y = paf_max, x = 0, color = element_type)) +
  geom_boxplot() +
  geom_jitter() +
  scale_y_log10("msPAF", breaks = 10 ^ seq(from = -4, to = 0, by = 0.25), labels = scales::percent) +
  ggtitle("Distribution of maximum observed risks")

mutate(sim$rc$output, element_type = "clusters") |>
  union(mutate(sim$rd$output, element_type = "ditches")) |>
  union(mutate(sim$rl$output, element_type = "lake")) |>
  filter("2013-04-20" < date, date < "2013-11-30") |>
  summarise(paf_max = max(paf_chronic, na.rm = TRUE), .by = c(element_id, tmoa, element_type)) |>
  ggplot(aes(y = paf_max, x = tmoa, color = element_type)) +
  geom_boxplot() +
  scale_y_log10("PAF", breaks = 10 ^ seq(from = -5, to = 0, by = 0.25), labels = scales::percent) +
  #scale_y_continuous("PAF", labels = scales::percent) +
  xlab("TMoA") +
  ggtitle("Distribution of maximum observed risks - by pesticide")




sim$rl$output |>
  filter("2013-04-20" < date, date < "2013-11-30") |>
  mutate(neg_log_surv = neg_log_surv(paf_chronic)) |>
  ggplot(aes(y = neg_log_surv, x = date, fill = tmoa, color = tmoa)) +
  geom_area() +
  scale_y_continuous(
    "-log(1-msPAF)",
    sec.axis = sec_axis(~ neg_log_surv_inv(.), name = "msPAF", labels = scales::percent)
  ) +
  NULL

sim$rl$output |>
  filter("2013-04-20" < date, date < "2013-11-30") |>
  summarise(mspaf = 1-prod(1-paf_chronic), .by = c(element_id, date)) |>
  mutate(neg_log_surv = neg_log_surv(mspaf)) |>
  ggplot(aes(y = neg_log_surv, x = date)) +
  geom_line() +
  #geom_line() +
  #scale_y_log10() +
  scale_y_continuous(
    "-log(1-msPAF)",
    sec.axis = sec_axis(~ neg_log_surv_inv(.), name = "msPAF", labels = scales::percent)
  ) +
  NULL




sim$rd$output |>
  filter("2013-04-20" < date, date < "2013-11-30") |>
  summarise(mspaf = 1-prod(1-paf_chronic), .by = c(element_id, date)) |>
  mutate(neg_log_surv = neg_log_surv(mspaf)) |>
  ggplot(aes(y = neg_log_surv, x = date, color = element_id)) +
  geom_line() +
  #geom_line() +
  #scale_y_log10() +
  scale_y_continuous(
    "-log(1-msPAF)",
    sec.axis = sec_axis(~ neg_log_surv_inv(.), name = "msPAF", labels = scales::percent)
  ) +
  NULL
