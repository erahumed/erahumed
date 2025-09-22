get_input <- function(simulation, parameter) {
  simulation [["inputs"]] [[parameter]]
}

get_raw_output <- function(simulation, layer) {
  res <- simulation [["outputs"]] [[layer]]

  cls <- paste0("erahumed_output_", layer)
  class(res) <- c(cls, class(res))

  res
}

get_etc <- function(simulation, name) {
  simulation [["etc"]] [[name]]
}
