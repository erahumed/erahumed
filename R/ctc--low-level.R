.compute_ctc <- function(simulation) {
  .par <- get_layer_parameters(simulation, "ctc")

  get_layer_output(simulation, "ca") |>
    data.table::as.data.table() |>
    merge(get_layer_output(simulation, "inp"), by = "date", sort = TRUE) |>  # to recover weather data
    collapse::rsplit(by = ~ cluster_id,
                     flatten = TRUE,
                     use.names = FALSE,
                     simplify = FALSE,
                     keep.by = TRUE
                     ) |>
    lapply(.compute_ctc_one_cluster,
           drift = .par[["drift"]],
           covmax = .par[["covmax"]],
           jgrow = .par[["jgrow"]],
           SNK = .par[["SNK"]],
           dact_m = .par[["dact_m"]],
           css_ppm = .par[["css_ppm"]],
           foc = .par[["foc"]],
           bd_g_cm3 = .par[["bd_g_cm3"]],
           qseep_m_day = .par[["qseep_m_day"]],
           wilting = .par[["wilting"]],
           fc = .par[["fc"]]
           ) |>
      data.table::rbindlist() |>
      as.data.frame() |>
      (\(.) { names(.)[names(.) == "cluster_id"] <- "element_id"; . })()
}

.compute_ctc_one_cluster <- function(cluster_ca_df,
                                     drift,
                                     covmax,
                                     jgrow,
                                     SNK,
                                     dact_m,
                                     css_ppm,
                                     foc,
                                     bd_g_cm3,
                                     qseep_m_day,
                                     wilting,
                                     fc
                                     )
{
  area_m2 <- cluster_ca_df[["area_m2"]][[1]]

  res_template <- as.list(cluster_ca_df[, c("cluster_id", "date")])

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
        chemical = chemical,
        drift = drift,
        covmax = covmax,
        jgrow = jgrow,
        SNK = SNK,
        dact_m = dact_m,
        css_ppm = css_ppm,
        foc = foc,
        bd_g_cm3 = bd_g_cm3,
        qseep_m_day = qseep_m_day,
        wilting = wilting,
        fc = fc
        )
      c(res_template, list(chemical = chemical), masses)
    }) |>
    data.table::rbindlist() |>
    as.data.frame()
}




