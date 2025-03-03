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
  output <- .compute_ctl(simulation)

  validate_ctl_output(output)

  simulation [["ctl"]] [["output"]] <- output

  return(simulation)

}



validate_ctl_output <- assert_data.frame
