.compute_ctc <- function(simulation) {
  applications_df <- get_etc(simulation, "applications_df")

  # Compute cluster's hydrology split by cluster
  hbc_split <- get_raw_output(simulation, "hbc") |>
    data.table::as.data.table() |>
    data.table::setorderv(c("element_id", "date")) |>
    # This merge serves to recover weather data (viz. temperatures)
    merge(get_raw_output(simulation, "inp"), by = "date", sort = TRUE) |>
    # This merge is only used to recover the harvesting column
    merge(get_etc(simulation, "management_df")[, c("seed_day", "tancat", "rfms_id", "rfms_name", "harvesting")],
          by = c("seed_day", "tancat", "rfms_id", "rfms_name"),  # by 'rfms_name' to avoid duplicating column
          sort = FALSE
          ) |>
    collapse::rsplit(by = ~ element_id, keep.by = TRUE)

  chem_db <- get_etc(simulation, "chemical_db")

  lapply(seq_along(chem_db), function(chemical_id) {
    lapply(hbc_split, \(cluster_ts_df) {
      element_id <- cluster_ts_df[["element_id"]][[1]]
      area_m2 <- cluster_ts_df[["area_m2"]][[1]]
      rfms_id <- cluster_ts_df[["rfms_id"]][[1]]
      rfms_name <- cluster_ts_df[["rfms_name"]][[1]]
      date <- cluster_ts_df[["date"]]
      chemical_name <- ct_get_param(chemical_id, "display_name", chem_db)

      ct_ts_df <- ct_time_series(
        application_kg = get_application_kg_vector(
          chemical = chemical_id,
          rfms_id = rfms_id,
          applications_df = applications_df,
          seed_day = cluster_ts_df[["seed_day"]],
          plan_delay = cluster_ts_df[["plan_delay"]],
          area_m2 = area_m2
          ),
        precipitation_mm = cluster_ts_df[["precipitation_mm"]],
        etp_mm = cluster_ts_df[["evapotranspiration_mm"]],
        temperature_ave_celsius = cluster_ts_df[["temperature_ave_celsius"]],
        temperature_min_celsius = cluster_ts_df[["temperature_min_celsius"]],
        temperature_max_celsius = cluster_ts_df[["temperature_max_celsius"]],
        volume_eod_m3 = area_m2 * cluster_ts_df[["height_eod_cm"]] / 100,
        outflow_m3 = cluster_ts_df[["outflow_m3"]],
        inflows_m3 = list(cluster_ts_df[["inflow_m3"]]),  # Clusters have a single source of inflow...
        inflows_densities_kg_m3 = list(0),  # ... which is assumed to be free of pesticide.
        area_m2 = area_m2,
        seed_day = cluster_ts_df[["seed_day"]],  # Used to estimate foliage surface growth
        harvesting = cluster_ts_df[["harvesting"]],
        simulation = simulation,
        chemical_id = chemical_id
      )
      c(list(element_id = element_id,
             rfms_id = rfms_id, rfms_name = rfms_name,
             chemical_id = chemical_id, chemical_name = chemical_name),
        list(date = date),
        ct_ts_df
        )
    }) |>
    data.table::rbindlist()
  }) |>
  data.table::rbindlist() |>
  as.data.frame()

}





