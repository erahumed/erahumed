.compute_ctd <- function(simulation) {
  output <- lapply(ctd_data_prep(simulation),
                   .compute_ctd_one_ditch,
                   simulation = simulation
                   ) |>
    data.table::rbindlist() |>
    as.data.frame() |>
    (\(.) { names(.)[names(.) == "ditch"] <- "element_id"; . })()
}

.compute_ctd_one_ditch <- function(ctd_preproc_data, simulation)
{
  res_template <- as.list(ctd_preproc_data[["hbd"]][, c("ditch", "date")])
  chemicals <- names(ctd_preproc_data[["cluster_inflows_densities_kg_m3"]])

  lapply(chemicals, function(chemical) {
      masses <- ct_time_series(
        application_kg = 0,  # Pesticide is applied only to rice field clusters
        precipitation_mm = 0,  # In our hydrology model, ditches are enclosed ..
        etp_mm = 0,  # .. in pipes, no volume changes from precipitation or ETP!
        temperature_ave = ctd_preproc_data[["inp"]][["temperature_ave"]],
        temperature_min = ctd_preproc_data[["inp"]][["temperature_min"]],
        temperature_max = ctd_preproc_data[["inp"]][["temperature_max"]],
        volume_eod_m3 = ctd_preproc_data[["hbd"]][["volume_m3"]],
        outflow_m3_s = ctd_preproc_data[["hbd"]][["outflow_lake_m3"]] / s_per_day(),
        inflows_m3_s =  # Ditch inflow waters come from clusters as well as from outside...
          c(ctd_preproc_data[["cluster_inflows_m3_s"]],
            list(ctd_preproc_data[["hbd"]][["inflow_external_m3"]] / s_per_day())
          ),
        inflows_densities_kg_m3 =  # ... the latters are assumed to be free of pesticide.
          c(ctd_preproc_data[["cluster_inflows_densities_kg_m3"]][[chemical]],
            list(0)
          ),
        area_m2 = ctd_preproc_data[["hbd"]][["surface"]][[1]],
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
    as.data.frame()
}



ctd_data_prep <- function(simulation)
{
  cluster_inflows_m3_s <-
    get_output(simulation, "hbc") |>
    data.table::as.data.table() |>
    data.table::setorderv(c("date", "cluster_id")) |>
    collapse::rsplit(by = outflow_m3_s ~ ditch + cluster_id,
                     flatten = FALSE,
                     use.names = TRUE,
                     simplify = TRUE,
                     keep.by = FALSE)

  cluster_inflows_densities_kg_m3 <-
    get_output(simulation, "ctc") |>
    data.table::as.data.table() |>
    merge(info_clusters(), by.x = "element_id", by.y = "cluster_id") |>
    data.table::setorderv(c("date", "element_id")) |>
    collapse::rsplit(by = cw_outflow_kg_m3 ~ ditch + chemical + element_id,
                     flatten = FALSE,
                     use.names = TRUE,
                     simplify = TRUE,
                     keep.by = FALSE)

  hbd_output_split <- get_output(simulation, "hbd") |>
    data.table::as.data.table() |>
    data.table::setorderv("date") |>
    merge(info_ditches(), by = "ditch") |>
    collapse::rsplit(by = ~ ditch,
                     flatten = FALSE,
                     use.names = TRUE,
                     simplify = FALSE,
                     keep.by = TRUE)

  inp_output <- get_output(simulation, "inp") |>
    data.table::as.data.table() |>
    data.table::setorderv("date")
  # This data-set has 1 more row (last day), so we drop it here to make
  # time-series lengths match
  inp_output <- inp_output[-nrow(inp_output),]  # Drop last row to make dates coincide with other dfs

  ditches <- names(hbd_output_split)

  lapply(ditches, function(ditch) {
    list(hbd = hbd_output_split[[ditch]],
         cluster_inflows_m3_s = cluster_inflows_m3_s[[ditch]],
         cluster_inflows_densities_kg_m3 = cluster_inflows_densities_kg_m3[[ditch]],
         inp = inp_output)
    })
}
