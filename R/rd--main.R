setup_rd <- function(simulation)
{
  tryCatch({
    assert_erahumed_simulation(simulation)
  },
  error = function(e) {
    class(e) <- c("validate_rd_params_error", class(e))
    stop(e)
  })

  setup_layer(layer = "rd")
}



compute_rd <- function(simulation)
{
  output <- risk_from_ssds(ct_output = get_layer_output(simulation, "ctd"))

  validate_rd_output(output)

  simulation [["rd"]] [["output"]] <- output

  return(simulation)
}



validate_rd_output <- assert_data.frame
