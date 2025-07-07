compute_rd <- function(simulation)
{
  output <- risk_from_ssds(ct_output = get_output(simulation, "ctd"),
                           chemical_db = get_etc(simulation, "chemical_db")
                           )

  validate_rd_output(output)

  simulation [["outputs"]] [["rd"]] <- output

  return(simulation)
}



validate_rd_output <- assert_data.frame
