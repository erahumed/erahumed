get_input <- function(simulation, parameter) {
  simulation [["inputs"]] [[parameter]]
}

get_output <- function(simulation, layer) {
  simulation [["outputs"]] [[layer]]
}

get_etc <- function(simulation, name) {
  simulation [["etc"]] [[name]]
}
