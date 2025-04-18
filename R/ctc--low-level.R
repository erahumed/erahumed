.compute_ctc <- function(simulation) {
  get_output(simulation, "ca") |>
    data.table::as.data.table() |>
    merge(get_output(simulation, "inp"), by = "date", sort = TRUE) |>  # to recover weather data
    merge(get_input(simulation, "management_df")[, c("seed_day", "tancat", "variety", "harvesting")],
          by = c("seed_day", "tancat", "variety"),  # TODO: To recover harvest day
          sort = FALSE
          ) |>
    collapse::rsplit(by = ~ element_id,
                     flatten = TRUE,
                     use.names = FALSE,
                     simplify = FALSE,
                     keep.by = TRUE
                     ) |>
    lapply(.compute_ctc_one_cluster,
           drift = get_input(simulation, "drift"),
           covmax = get_input(simulation, "covmax"),
           jgrow = get_input(simulation, "jgrow"),
           dact_m = get_input(simulation, "dact_m"),
           css_ppm = get_input(simulation, "css_ppm"),
           foc = get_input(simulation, "foc"),
           bd_g_cm3 = get_input(simulation, "bd_g_cm3"),
           qseep_m_day = get_input(simulation, "qseep_m_day"),
           porosity = get_input(simulation, "porosity")
           ) |>
      data.table::rbindlist() |>
      as.data.frame()
}

.compute_ctc_one_cluster <- function(cluster_ca_df,
                                     drift,
                                     covmax,
                                     jgrow,
                                     dact_m,
                                     css_ppm,
                                     foc,
                                     bd_g_cm3,
                                     qseep_m_day,
                                     porosity
                                     )
{
  area_m2 <- cluster_ca_df[["area_m2"]][[1]]

  res_template <- as.list(cluster_ca_df[, c("element_id", "date")])

  chemicals <- unique(erahumed::albufera_ca_schedules$chemical)
  chemicals <- names(cluster_ca_df)[names(cluster_ca_df) %in% chemicals]

  lapply(chemicals, function(chemical) {
      masses <- ct_time_series(
        application_kg = cluster_ca_df[[chemical]],  # Chemicals are columns in the CA output
        precipitation_mm = cluster_ca_df[["precipitation_mm"]],
        etp_mm = cluster_ca_df[["evapotranspiration_mm"]],
        temperature_ave = cluster_ca_df[["temperature_ave"]],
        temperature_min = cluster_ca_df[["temperature_min"]],
        temperature_max = cluster_ca_df[["temperature_max"]],
        volume_eod_m3 = area_m2 * cluster_ca_df[["height_eod_cm"]] / 100,
        outflow_m3_s = cluster_ca_df[["outflow_m3_s"]],
        inflows_m3_s = list(cluster_ca_df[["inflow_m3_s"]]),  # Clusters have a single source of inflow...
        inflows_densities_kg_m3 = list(0),  # ... which is assumed to be free of pesticide.
        area_m2 = area_m2,
        seed_day = cluster_ca_df[["seed_day"]],  # Used to estimate foliage surface growth
        harvesting = cluster_ca_df[["harvesting"]],
        chemical = chemical,
        drift = drift,
        covmax = covmax,
        jgrow = jgrow,
        dact_m = dact_m,
        css_ppm = css_ppm,
        foc = foc,
        bd_g_cm3 = bd_g_cm3,
        qseep_m_day = qseep_m_day,
        porosity = porosity
        )
      c(res_template, list(chemical = chemical), masses)
    }) |>
    data.table::rbindlist() |>
    as.data.frame()
}




