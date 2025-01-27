ctd_to_ditch_wrap <- function(ctd_preproc_data, ctd_params)
{
  res_template <- list(
    ditch = ctd_preproc_data[["hbd"]][["ditch"]],
    date = ctd_preproc_data[["inp"]][["date"]]
  )

  compute_masses <- function(chemical) {
    ct_time_series(
      application_kg = 0,
      precipitation_mm = ctd_preproc_data[["inp"]][["precipitation_mm"]],
      etp_mm = ctd_preproc_data[["inp"]][["evapotranspiration_mm"]],
      temperature_ave = ctd_preproc_data[["inp"]][["temperature_ave"]],
      temperature_min = ctd_preproc_data[["inp"]][["temperature_min"]],
      temperature_max = ctd_preproc_data[["inp"]][["temperature_max"]],
      height_eod_cm = ctd_preproc_data[["hbd"]][["height_eod_cm"]],
      outflow_m3_s = ctd_preproc_data[["hbd"]][["outflow_lake_m3"]] / s_per_day(),
      inflows_m3_s_kg = ctd_preproc_data[["ctc"]][[chemical]],
      area_m2 = 0,
      seed_day = 150,  # Irrelevant since application_kg = 0
      chemical = chemical,
      drift = ctd_params[["drift"]],
      covmax = ctd_params[["covmax"]],
      jgrow = ctd_params[["jgrow"]],
      SNK = ctd_params[["SNK"]],
      dact_m = ctd_params[["dact_m"]],
      css_ppm = ctd_params[["css_ppm"]],
      foc = ctd_params[["foc"]],
      bd_g_cm3 = ctd_params[["bd_g_cm3"]],
      qseep_m_day = ctd_params[["qseep_m_day"]],
      wilting = ctd_params[["wilting"]],
      fc = ctd_params[["fc"]]
    )
  }

  chemicals <- names(ctd_preproc_data[["ctc"]])

  lapply(chemicals, \(chemical)
         c(res_template, list(chemical = chemical), compute_masses(chemical))
  ) |>
    data.table::rbindlist() |>
    as.data.frame()
}



ctd_data_prep <- function(simulation)
{
  ctc_output_split <- get_layer_output(simulation, "ctc") |>
    data.table::as.data.table() |>
    merge(info_clusters(), by = "cluster_id") |>
    merge(get_layer_output(simulation, "hbc"), by = c("cluster_id", "date")) |>
    data.table::setorderv("date") |>
    collapse::rsplit(by = outflow_m3_s + cw_outflow ~ ditch + chemical + cluster_id,
                     flatten = FALSE,
                     use.names = TRUE,
                     simplify = FALSE,
                     keep.by = TRUE)

  hbd_output_split <- get_layer_output(simulation, "hbd") |>
    data.table::as.data.table() |>
    data.table::setorderv("date") |>
    collapse::rsplit(by = ~ ditch,
                     flatten = FALSE,
                     use.names = TRUE,
                     simplify = FALSE,
                     keep.by = TRUE)

  inp_output <- get_layer_output(simulation, "inp") |>
    data.table::as.data.table() |>
    data.table::setorderv("date")

  stopifnot(setequal(names(ctc_output_split), names(hbd_output_split)))

  ditches <- names(ctc_output_split)

  lapply(ditches, function(ditch) {
    list(hbd = hbd_output_split[[ditch]],
         ctc = ctc_output_split[[ditch]],
         inp = inp_output)
    })
}
