.compute_ctd <- function(simulation) {
  output <- lapply(ctd_data_prep(simulation),
                   .compute_ctd_one_ditch,
                   ctd_params = get_layer_parameters(simulation, "ctd")
                   ) |>
    data.table::rbindlist() |>
    as.data.frame()
}

.compute_ctd_one_ditch <- function(ctd_preproc_data, ctd_params)
{
  res_template <- as.list(ctd_preproc_data[["hbd"]][, c("ditch", "date")])
  chemicals <- names(ctd_preproc_data[["cluster_inflows_densities_kg_m3"]])


  compute_masses <- function(chemical) {

  }


  lapply(chemicals, function(chemical) {
      masses <- ct_time_series(
        application_kg = 0,
        precipitation_mm = 0,
        etp_mm = 0,
        temperature_ave = ctd_preproc_data[["inp"]][["temperature_ave"]],
        temperature_min = ctd_preproc_data[["inp"]][["temperature_min"]],
        temperature_max = ctd_preproc_data[["inp"]][["temperature_max"]],
        height_eod_cm = ctd_preproc_data[["hbd"]][["height_eod_cm"]],
        outflow_m3_s = ctd_preproc_data[["hbd"]][["outflow_lake_m3"]] / s_per_day(),
        inflows_m3_s =
          c(ctd_preproc_data[["cluster_inflows_m3_s"]],
            list(ctd_preproc_data[["hbd"]][["inflow_external_m3"]] / s_per_day())
          ),
        inflows_densities_kg_m3 =
          c(ctd_preproc_data[["cluster_inflows_densities_kg_m3"]][[chemical]],
            list(0)
          ),
        area_m2 = ctd_preproc_data[["hbd"]][["surface"]][[1]],
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
      c(res_template, list(chemical = chemical), masses)
    }) |>
    data.table::rbindlist() |>
    as.data.frame()
}



ctd_data_prep <- function(simulation)
{
  hbc_output <-
    get_layer_output(simulation, "hbc")[, c("cluster_id", "date", "outflow_m3_s")]

  ctc_output_split <-
    get_layer_output(simulation, "ctc") |>
    data.table::as.data.table() |>
    merge(info_clusters(), by = "cluster_id") |>
    merge(hbc_output, by = c("cluster_id", "date")) |>
    data.table::setorderv("date")

  cluster_inflows_m3_s <-
    get_layer_output(simulation, "hbc") |>
    data.table::as.data.table() |>
    data.table::setorderv(c("date", "cluster_id")) |>
    collapse::rsplit(by = outflow_m3_s ~ ditch + cluster_id,
                     flatten = FALSE,
                     use.names = TRUE,
                     simplify = TRUE,
                     keep.by = FALSE)

  cluster_inflows_densities_kg_m3 <-
    get_layer_output(simulation, "ctc") |>
    data.table::as.data.table() |>
    merge(info_clusters(), by = "cluster_id") |>
    data.table::setorderv(c("date", "cluster_id")) |>
    collapse::rsplit(by = cw_outflow ~ ditch + chemical + cluster_id,
                     flatten = FALSE,
                     use.names = TRUE,
                     simplify = TRUE,
                     keep.by = FALSE)

  hbd_output_split <- get_layer_output(simulation, "hbd") |>
    data.table::as.data.table() |>
    data.table::setorderv("date") |>
    merge(info_ditches(), by = "ditch") |>
    collapse::rsplit(by = ~ ditch,
                     flatten = FALSE,
                     use.names = TRUE,
                     simplify = FALSE,
                     keep.by = TRUE)

  inp_output <- get_layer_output(simulation, "inp") |>
    data.table::as.data.table() |>
    data.table::setorderv("date")
  inp_output <- inp_output[-nrow(inp_output),]  # Drop last row to make dates coincide with other dfs

  ditches <- names(hbd_output_split)

  lapply(ditches, function(ditch) {
    list(hbd = hbd_output_split[[ditch]],
         cluster_inflows_m3_s = cluster_inflows_m3_s[[ditch]],
         cluster_inflows_densities_kg_m3 = cluster_inflows_densities_kg_m3[[ditch]],
         inp = inp_output)
    })
}
