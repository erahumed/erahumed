setup_ctl <- function(
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
    class(e) <- c("validate_ctl_params_error", class(e))
    stop(e)
  })

  setup_layer(layer = "ctl")
}



compute_ctl <- function(simulation)
{
  drift <- get_layer_parameters(simulation, "ctl")[["drift"]]
  covmax <- get_layer_parameters(simulation, "ctl")[["covmax"]]
  jgrow <- get_layer_parameters(simulation, "ctl")[["jgrow"]]
  SNK <- get_layer_parameters(simulation, "ctl")[["SNK"]]
  dact_m <- get_layer_parameters(simulation, "ctl")[["dact_m"]]
  css_ppm <- get_layer_parameters(simulation, "ctl")[["css_ppm"]]
  foc <- get_layer_parameters(simulation, "ctl")[["foc"]]
  bd_g_cm3 <- get_layer_parameters(simulation, "ctl")[["bd_g_cm3"]]
  qseep_m_day <- get_layer_parameters(simulation, "ctl")[["qseep_m_day"]]
  wilting <- get_layer_parameters(simulation, "ctl")[["wilting"]]
  fc <- get_layer_parameters(simulation, "ctl")[["fc"]]

  output <- NULL |>
    as.data.frame()

  validate_ctl_output(output)

  simulation [["ctl"]] [["output"]] <- output

  return(simulation)

}



validate_ctl_output <- assert_data.frame
