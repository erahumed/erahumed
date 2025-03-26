compute_rc <- function(simulation)
{
  output <- risk_from_ssds(ct_output = get_output(simulation, "ctc"))

  validate_rc_output(output)

  simulation [["outputs"]] [["rc"]] <- output

  return(simulation)
}



validate_rc_output <- assert_data.frame
