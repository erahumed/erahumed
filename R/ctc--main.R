compute_ctc <- function(simulation)
{
  output <- .compute_ctc(simulation)

  validate_ctc_output(output)

  simulation [["outputs"]] [["ctc"]] <- output

  return(simulation)
}



validate_ctc_output <- assert_data.frame
