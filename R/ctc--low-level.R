.compute_ctc <- function(simulation) {
  applications_df <- get_etc(simulation, "applications_df")

  # Compute cluster's hydrology split by cluster
  hbc_split <- get_output(simulation, "hbc") |>
    data.table::as.data.table() |>
    data.table::setorderv(c("element_id", "date")) |>
    # This merge serves to recover weather data (viz. temperatures)
    merge(get_output(simulation, "inp"), by = "date", sort = TRUE) |>
    # This merge is only used to recover the harvesting column
    merge(get_etc(simulation, "management_df")[, c("seed_day", "tancat", "variety", "harvesting")],
          by = c("seed_day", "tancat", "variety"),
          sort = FALSE
          ) |>
    collapse::rsplit(by = ~ element_id, keep.by = TRUE)

  lapply(seq_along(get_etc(simulation, "chemical_db")), function(chemical_id) {
    lapply(hbc_split, \(cluster_ts_df) {
      element_id <- cluster_ts_df[["element_id"]][[1]]
      area_m2 <- cluster_ts_df[["area_m2"]][[1]]
      variety <- cluster_ts_df[["variety"]][[1]]
      date <- cluster_ts_df[["date"]]

      ct_ts_df <- ct_time_series(
        application_kg = get_application_kg_vector(
          chemical = chemical_id,
          variety = variety,
          applications_df = applications_df,
          seed_day = cluster_ts_df[["seed_day"]],
          plan_delay = cluster_ts_df[["plan_delay"]],
          area_m2 = area_m2
          ),
        precipitation_mm = cluster_ts_df[["precipitation_mm"]],
        etp_mm = cluster_ts_df[["evapotranspiration_mm"]],
        temperature_ave = cluster_ts_df[["temperature_ave"]],
        temperature_min = cluster_ts_df[["temperature_min"]],
        temperature_max = cluster_ts_df[["temperature_max"]],
        volume_eod_m3 = area_m2 * cluster_ts_df[["height_eod_cm"]] / 100,
        outflow_m3_s = cluster_ts_df[["outflow_m3_s"]],
        inflows_m3_s = list(cluster_ts_df[["inflow_m3_s"]]),  # Clusters have a single source of inflow...
        inflows_densities_kg_m3 = list(0),  # ... which is assumed to be free of pesticide.
        area_m2 = area_m2,
        seed_day = cluster_ts_df[["seed_day"]],  # Used to estimate foliage surface growth
        harvesting = cluster_ts_df[["harvesting"]],
        simulation = simulation,
        chemical_id = chemical_id
      )
      c(list(element_id = element_id, chemical_id = chemical_id),
        list(date = date),
        ct_ts_df
        )
    }) |>
    data.table::rbindlist()
  }) |>
  data.table::rbindlist() |>
  as.data.frame()

}





