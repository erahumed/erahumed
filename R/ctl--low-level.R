.compute_ctl <- function(simulation)
{
  ctl_preproc_data <- ctl_data_prep(simulation)

  res_template <- list(date = ctl_preproc_data[["inp"]][["date"]])

  chemicals <- unique(erahumed::albufera_ca_schedules$chemical)

  lapply(chemicals, function(chemical) {
      # Lake surface used for P-ETP calculations is the relevant one here,
      # because of the internal logic of ct_time_series()
      ls <- get_input(simulation, "petp_surface_m2")

      masses <- ct_time_series(
        application_kg = 0,  # Pesticide is applied only to rice field clusters
        precipitation_mm = ctl_preproc_data[["hbl"]][["precipitation_mm"]],
        etp_mm = ctl_preproc_data[["inp"]][["evapotranspiration_mm"]],
        temperature_ave = ctl_preproc_data[["inp"]][["temperature_ave"]],
        temperature_min = ctl_preproc_data[["inp"]][["temperature_min"]],
        temperature_max = ctl_preproc_data[["inp"]][["temperature_max"]],
        volume_eod_m3 = ctl_preproc_data[["hbl"]][["volume_eod"]],
        outflow_m3_s = ctl_preproc_data[["hbl"]][["outflow_total"]],
        inflows_m3_s = ctl_preproc_data[["ditch_inflows_m3_s"]],  # Lake Inflow = ditches outflows
        inflows_densities_kg_m3 =
          ctl_preproc_data[["ditch_inflows_densities_kg_m3"]][[chemical]],
        area_m2 = ls,
        seed_day = 840,  # Ignored, only relevant for application interception (in clusters)
        chemical = chemical,
        drift = get_input(simulation, "drift"),
        covmax = get_input(simulation, "covmax"),
        jgrow = get_input(simulation, "jgrow"),
        dact_m = get_input(simulation, "dact_m"),
        css_ppm = get_input(simulation, "css_ppm"),
        foc = get_input(simulation, "foc"),
        bd_g_cm3 = get_input(simulation, "bd_g_cm3"),
        qseep_m_day = get_input(simulation, "qseep_m_day"),
        porosity = get_input(simulation, "porosity")
        )
      c(res_template, list(chemical = chemical), masses)
    }) |>
    data.table::rbindlist() |>
    as.data.frame() |>
    (\(.) { .$element_id <- "lake"; . })()
}



ctl_data_prep <- function(simulation)
{
  ditch_inflows_m3_s <-
    get_output(simulation, "hbd") |>
    data.table::as.data.table() |>
    data.table::setorderv(c("date", "element_id")) |>
    collapse::rsplit(by = outflow_lake_m3 ~ element_id,
                     simplify = TRUE,
                     keep.by = FALSE) |>
    lapply(\(x) x / s_per_day())

  ditch_inflows_densities_kg_m3 <-
    get_output(simulation, "ctd") |>
    data.table::as.data.table() |>
    data.table::setorderv(c("date", "element_id")) |>
    collapse::rsplit(by = cw_outflow_kg_m3 ~ chemical + element_id,
                     flatten = FALSE,
                     simplify = TRUE,
                     keep.by = FALSE)

  hbl_output <- get_output(simulation, "hbl") |>
    data.table::as.data.table() |>
    data.table::setorderv("date")

  inp_output <- get_output(simulation, "inp") |>  # Used to recover weather data
    data.table::as.data.table() |>
    data.table::setorderv("date")
  # This data-set has 1 more row (last day), so we drop it here to make
  # time-series lengths match
  inp_output <- inp_output[-nrow(inp_output),]

  list(hbl = hbl_output,
       ditch_inflows_m3_s = ditch_inflows_m3_s,
       ditch_inflows_densities_kg_m3 = ditch_inflows_densities_kg_m3,
       inp = inp_output)

}
