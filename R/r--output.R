materialize_r_output <- function(output) {

  data.frame(
    date = output$date,
    element_id = output$element_id,
    stressor = output$stressor_name,
    stressor_type = output$stressor_type,
    rq_acute = output$rq_acute,
    rq_chronic = output$rq_chronic,
    paf_acute_one = output$paf_acute,
    paf_chronic_one = output$paf_chronic
  )

}

#' @export
materialize_output.erahumed_output_rl <- function(output) {
  materialize_r_output(output)
}

#' @export
materialize_output.erahumed_output_rd <- function(output) {
  materialize_r_output(output)
}

#' @export
materialize_output.erahumed_output_rc <- function(output) {
  materialize_r_output(output)
}
