get_input <- function(simulation, parameter) {
  simulation [["inputs"]] [[parameter]]
}

get_raw_output <- function(simulation, layer) {
  res <- simulation [["outputs"]] [[layer]]

  cls <- paste0("erahumed_output_", layer)
  class(res) <- c(cls, "erahumed_output", class(res))

  res
}

materialize_output <- function(output) {
  UseMethod("materialize_output")
}

#' @export
materialize_output.erahumed_output <- function(output) {
  return( output )
}


get_etc <- function(simulation, name) {
  simulation [["etc"]] [[name]]
}
