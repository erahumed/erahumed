library(erahumed)
library(dplyr)
library(ggplot2)

date_min <- "2020-01-01"
date_max <- "2020-12-31"

outflows_df <- albufera_outflows |>
  (\(.) .[date_min <= .$date & .$date <= date_max, ])()

df <- erahumed_simulation() |>
  setup_inp(outflows_df = outflows_df) |>
  run_simulation("hbc") |>
  get_layer_output("hbc") |>
  select(cluster_id, variety, area_m2, tancat, ditch) |>
  unique()



# Check that proportions are JSendra:Bomba:Clearfield = 80:10:10 as expected
df |>
  summarise(area = sum(area_m2), .by = variety) |>
  mutate(area_prop = area / sum(area))

# Check that Bomba is only cultivated in tancats
df |>
  filter(variety == "Bomba", !tancat) |>
  nrow()  # Should be zero

# Check that Clearfield is only cultivated in ditches 1:19
df |>
  filter(variety == "Clearfield",
         !(ditch %in% paste0("d", 1:19))
  ) |>
  nrow()  # Should be zero

# Plot distribution of ditch according to cluster id

df |>
  summarise(clusters = n_distinct(cluster_id), .by = c(ditch, variety)) |>
  ggplot(aes(x = ditch, y = clusters, fill = variety)) +
  geom_col(position = "identity", alpha = 0.5)
