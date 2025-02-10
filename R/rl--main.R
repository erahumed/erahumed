setup_rl <- function(simulation)
{
  tryCatch({
    assert_erahumed_simulation(simulation)
  },
  error = function(e) {
    class(e) <- c("validate_rl_params_error", class(e))
    stop(e)
  })

  setup_layer(layer = "rl")
}



compute_rl <- function(simulation)
{
  output <- risk_from_ssds(ct_output = get_layer_output(simulation, "ctl"))

  validate_rl_output(output)

  simulation [["rl"]] [["output"]] <- output

  return(simulation)
}



validate_rl_output <- assert_data.frame
