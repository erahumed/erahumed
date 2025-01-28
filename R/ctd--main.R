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
  output <-
    lapply(
      ctd_data_prep(simulation),
      ctd_to_ditch_wrap,
      ctd_params = get_layer_parameters(simulation, "ctd")
      ) |>
    data.table::rbindlist() |>
    as.data.frame()

  validate_ctd_output(output)

  simulation [["ctd"]] [["output"]] <- output

  return(simulation)
}



validate_ctd_output <- assert_data.frame
