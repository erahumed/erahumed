compute_ctd <- function(simulation)
{

  output <- .compute_ctd(simulation)

  validate_ctd_output(output)

  simulation [["outputs"]] [["ctd"]] <- output

  return(simulation)
}



validate_ctd_output <- assert_data.frame
