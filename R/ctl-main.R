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
  output <- ctl_wrap(
      ctl_data_prep(simulation),
      ctl_params = get_layer_parameters(simulation, "ctl")
    ) |>
    as.data.frame()

  validate_ctl_output(output)

  simulation [["ctl"]] [["output"]] <- output

  return(simulation)

}



validate_ctl_output <- assert_data.frame
