setup_ctd <- function(
    simulation,
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
  tryCatch({
    assert_erahumed_simulation(simulation)
  },
  error = function(e) {
    class(e) <- c("validate_ctd_params_error", class(e))
    stop(e)
  })

  setup_layer(layer = "ctd")
}



compute_ctd <- function(simulation)
{
  drift <- get_layer_parameters(simulation, "ctd")[["drift"]]
  covmax <- get_layer_parameters(simulation, "ctd")[["covmax"]]
  jgrow <- get_layer_parameters(simulation, "ctd")[["jgrow"]]
  SNK <- get_layer_parameters(simulation, "ctd")[["SNK"]]
  dact_m <- get_layer_parameters(simulation, "ctd")[["dact_m"]]
  css_ppm <- get_layer_parameters(simulation, "ctd")[["css_ppm"]]
  foc <- get_layer_parameters(simulation, "ctd")[["foc"]]
  bd_g_cm3 <- get_layer_parameters(simulation, "ctd")[["bd_g_cm3"]]
  qseep_m_day <- get_layer_parameters(simulation, "ctd")[["qseep_m_day"]]
  wilting <- get_layer_parameters(simulation, "ctd")[["wilting"]]
  fc <- get_layer_parameters(simulation, "ctd")[["fc"]]

  output <- get_layer_output(simulation, "ctc") |>
    data.table::as.data.table() |>
    merge(
      info_clusters() |> data.table::as.data.table(),
      by = "cluster_id") |>
    merge(
      get_layer_output(simulation, "hbd") |> data.table::as.data.table(),
      by = c("date", "ditch"),
      sort = TRUE
    ) |>
    collapse::rsplit(
      by = ~ ditch,
      flatten = TRUE,
      use.names = FALSE,
      simplify = FALSE,
      keep.by = TRUE
    ) |>
    # TODO
    data.table::rbindlist() |>
    as.data.frame()

  validate_ctd_output(output)

  simulation [["ctd"]] [["output"]] <- output

  return(simulation)
}



validate_ctd_output <- assert_data.frame
