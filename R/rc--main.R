setup_rc <- function(simulation)
{
  tryCatch({
    assert_erahumed_simulation(simulation)
  },
  error = function(e) {
    class(e) <- c("validate_rc_params_error", class(e))
    stop(e)
  })

  setup_layer(layer = "rc")
}



compute_rc <- function(simulation)
{
  output <- risk_from_ssds(ct_output = get_layer_output(simulation, "ctc"))

  validate_rc_output(output)

  simulation [["rc"]] [["output"]] <- output

  return(simulation)
}



validate_rc_output <- assert_data.frame
