materialize_ct_output <- function(output) {
  output$chemical <- output$chemical_name

  output$chemical_id <- NULL
  output$chemical_name <- NULL

  output$rfms_id <- NULL
  output$rfms_name <- NULL

  return(output)
}

#' @export
materialize_output.erahumed_output_ctl <- function(output) {
  materialize_ct_output(output)
}

#' @export
materialize_output.erahumed_output_ctd <- function(output) {
  materialize_ct_output(output)
}

#' @export
materialize_output.erahumed_output_ctc <- function(output) {
  materialize_ct_output(output)
}
