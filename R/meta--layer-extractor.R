get_simulation_layer <- function(simulation, layer = erahumed_layers()) {
  assert_erahumed_simulation(simulation)
  layer <- match.arg(layer)
  if (layer %in% names(simulation))
    return( simulation[[layer]] )
  return(NULL)
}
