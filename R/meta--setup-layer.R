setup_layer <- function(layer = erahumed_layers(),
                        validate_params = function(...) TRUE)
{
  layer <- match.arg(layer)

  lst <- unpack_setup_call()
  simulation <- lst$simulation
  params <- lst$params

  do.call(validate_params, params)

  simulation[[layer]] <-
    new_simulation_layer(params = params, layer_name = layer)

  for (downstream_layer in downstream_layers(layer))
    simulation <- reset_layer_output(simulation, downstream_layer)

  return(simulation)
}

unpack_setup_call <- function(call) {
  # This assumes unpack_setup_call() is always called by setup_layer(), which,
  # in turn, is always called by some actual layer's setup_*() function.
  pos <- -2
  fun <- sys.function(pos)
  envir <- sys.frame(pos)

  param_names <- setdiff(names(formals(fun)), "simulation")

  params <- mget(param_names, envir = envir)
  simulation <- get("simulation", envir = envir, inherits = FALSE)

  return( list(simulation = simulation, params = params) )
}
