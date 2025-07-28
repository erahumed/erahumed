# Plot the "time distribution" of applications of the various chemicals. For a
# visual check, comparing with scheduled times.

library(erahumed)
library(dplyr)
library(ggplot2)

hbl <- hbc(date_min = "2014-01-01", date_max = "2014-12-31")
ca_df <- ca(hbl)

ca_df |>
  select(date, rfms_id, all_of(unique(albufera_ca_schedules$chemical)) ) |>
  group_by(date, rfms_id) |>
  summarise(across(everything(), sum)) |>
  tidyr::pivot_longer(-c(date, rfms_id)) |>
  mutate(date = as.Date(date) - as.Date("2014-04-20")) |>
  ggplot(aes(x = date, y = value, color = name)) +
  geom_line() +
  facet_grid(rfms_id ~ .)

plotly::ggplotly()
