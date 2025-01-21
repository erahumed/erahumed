ctc_to_cluster_wrap <- function(cluster_ca_df,
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
  res_template <- list(
    cluster_id = cluster_ca_df[["cluster_id"]],
    date = cluster_ca_df[["date"]]
  )

  compute_masses <- function(chemical) {
    ct_time_series(
      application_kg = cluster_ca_df[[chemical]],
      precipitation_mm = cluster_ca_df[["precipitation_mm"]],
      etp_mm = cluster_ca_df[["evapotranspiration_mm"]],
      temperature_ave = cluster_ca_df[["temperature_ave"]],
      temperature_min = cluster_ca_df[["temperature_min"]],
      temperature_max = cluster_ca_df[["temperature_max"]],
      height_eod_cm = cluster_ca_df[["height_eod_cm"]],
      outflow_m3_s = cluster_ca_df[["outflow_m3_s"]],
      inflow_m3_s = cluster_ca_df[["inflow_m3_s"]],
      area_m2 = cluster_ca_df[["area_m2"]][[1]],
      seed_day = cluster_ca_df[["seed_day"]],
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
  }

  chemicals <- unique(erahumed::albufera_ca_schedules$chemical)
  chemicals <- names(cluster_ca_df)[names(cluster_ca_df) %in% chemicals]

  lapply(chemicals, \(chemical)
         c(res_template, list(chemical = chemical), compute_masses(chemical))
         ) |>
    data.table::rbindlist() |>
    as.data.frame()
}




