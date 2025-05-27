compute_rl <- function(simulation)
{
  output <- risk_from_ssds(ct_output = get_output(simulation, "ctl"),
                           chemical_db = get_etc(simulation, "chemical_db")
                           )

  validate_rl_output(output)

  simulation [["outputs"]] [["rl"]] <- output

  return(simulation)
}



validate_rl_output <- assert_data.frame
