.compute_ctd <- function(simulation) {
  cw_outflow_kg_m3 <- outflow_m3 <- NULL

  hbd_output_split <- get_output(simulation, "hbd") |>
    data.table::as.data.table() |>
    data.table::setorderv("date") |>
    merge(info_ditches(), by = "element_id") |>
    merge(get_output(simulation, "inp"), by = "date", sort = TRUE) |>
    collapse::rsplit(by = ~ element_id, keep.by = TRUE)

  cluster_inflows_df_list <-
    get_output(simulation, "ctc") |>
    data.table::as.data.table() |>
    merge(info_clusters(), by = "element_id") |>
    data.table::setorderv("date") |>
    collapse::rsplit(by = ~ ditch_element_id + element_id,
                     flatten = FALSE,
                     use.names = TRUE,
                     keep.by = FALSE)

  lapply(seq_along(get_etc(simulation, "chemical_db")), function(chemical_id) {
    lapply(hbd_output_split, function(ditch_ts_df) {
      element_id <- ditch_ts_df[["element_id"]][[1]]
      area_m2 <- ditch_ts_df[["surface"]][[1]]

      date <- ditch_ts_df[["date"]]

      cluster_inflows_df <- cluster_inflows_df_list[[ element_id ]]
      cluster_inflows_m3_s <- lapply(cluster_inflows_df, function(df) {
          idx <- df$chemical_id == chemical_id
          df[idx, outflow_m3] / s_per_day()
        })
      cluster_inflow_densities_kg_m3 <- lapply(cluster_inflows_df, function(df){
          idx <- df$chemical_id == chemical_id
          df[idx, cw_outflow_kg_m3]
        })

      ct_ts_df <- ct_time_series(
        application_kg = 0,  # Pesticide is applied only to rice field clusters
        precipitation_mm = 0,  # In our hydrology model, ditches are enclosed ..
        etp_mm = 0,  # .. in pipes, no volume changes from precipitation or ETP!
        temperature_ave = ditch_ts_df[["temperature_ave"]],
        temperature_min = ditch_ts_df[["temperature_min"]],
        temperature_max = ditch_ts_df[["temperature_max"]],
        volume_eod_m3 = ditch_ts_df[["volume_m3"]],
        outflow_m3_s = ditch_ts_df[["outflow_lake_m3"]] / s_per_day(),
        inflows_m3_s =  # Ditch inflow waters come from clusters as well as from outside...
          c(cluster_inflows_m3_s,
            list(ditch_ts_df[["inflow_external_m3"]] / s_per_day())
          ),
        inflows_densities_kg_m3 =  # ... the latters are assumed to be free of pesticide.
          c(cluster_inflow_densities_kg_m3, list(0)),
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
    data.table::rbindlist()
  }) |>
  data.table::rbindlist() |>
  as.data.frame()

}
