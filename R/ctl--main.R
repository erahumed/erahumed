compute_ctl <- function(simulation)
{
  output <- .compute_ctl(simulation)

  validate_ctl_output(output)

  simulation [["outputs"]] [["ctl"]] <- output

  return(simulation)

}



validate_ctl_output <- assert_data.frame
