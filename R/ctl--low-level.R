.compute_ctl <- function(simulation)
{
  cw_outflow_kg_m3 <- outflow_m3 <- NULL

  lake_ts_df <- get_output(simulation, "hbl") |>
    (\(.) .[, c("date", "volume_eod", "outflow_total")])() |>
    data.table::as.data.table() |>
    data.table::setorderv("date") |>
    merge(get_output(simulation, "inp"), by = "date")

  ditch_inflows_df_list <-
    get_output(simulation, "ctd") |>
    data.table::as.data.table() |>
    data.table::setorderv("date") |>
    collapse::rsplit(by = ~ element_id)

  lapply(seq_along(get_etc(simulation, "chemical_db")), function(chemical_id) {
      element_id <- "lake"
      date <- lake_ts_df[["date"]]
      area_m2 <- get_input(simulation, "petp_surface_m2")

      ditch_inflows_m3_s <- lapply(ditch_inflows_df_list, function(df)
        {
          idx <- df$chemical_id == chemical_id
          df[idx, outflow_m3] / s_per_day()
        })
      ditch_inflow_densities_kg_m3 <- lapply(ditch_inflows_df_list, function(df)
        {
          idx <- df$chemical_id == chemical_id
          df[idx, cw_outflow_kg_m3]
        })

      ct_ts_df <- ct_time_series(
        application_kg = 0,  # Pesticide is applied only to rice field clusters
        precipitation_mm = lake_ts_df[["precipitation_mm"]],
        etp_mm = lake_ts_df[["evapotranspiration_mm"]],
        temperature_ave = lake_ts_df[["temperature_ave"]],
        temperature_min = lake_ts_df[["temperature_min"]],
        temperature_max = lake_ts_df[["temperature_max"]],
        volume_eod_m3 = lake_ts_df[["volume_eod"]],
        outflow_m3_s = lake_ts_df[["outflow_total"]],
        inflows_m3_s = ditch_inflows_m3_s,
        inflows_densities_kg_m3 =  # ... the latters are assumed to be free of pesticide.
          c(ditch_inflow_densities_kg_m3, list(0)),
        area_m2 = area_m2,
        seed_day = 840,  # Ignored, only relevant for application interception (in clusters)
        harvesting = FALSE, # Ignore harvesting (do not change this value)
        simulation = simulation,
        chemical_id = chemical_id
      )
      c(list(element_id = element_id, chemical_id = chemical_id),
        list(date = date),
        ct_ts_df
      )
    }) |>
    data.table::rbindlist() |>
    as.data.frame()

}
