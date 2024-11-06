get_simulation_layer <- function(model, layer = erahumed_layers()) {
  assert_erahumed_simulation(model)
  layer <- match.arg(layer)
  if (layer %in% names(model))
    return( model[[layer]] )
  return(NULL)
}
