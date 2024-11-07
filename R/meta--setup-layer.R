setup_layer <- function(simulation,
                        layer = erahumed_layers(),
                        ...,
                        validate_params = function(...) TRUE
)
{
  assert_erahumed_simulation(simulation)
  layer <- match.arg(layer)
  validate_params(...)

  simulation[[layer]] <- new_simulation_layer(output = NULL,
                                              params = list(...),
                                              layer_name = layer
  )

  for (downstream_layer in downstream_layers(layer))
    simulation <- reset_layer_output(simulation, downstream_layer)

  return(simulation)
}
