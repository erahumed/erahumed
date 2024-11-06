#' @export
run_simulation <- function(simulation, layer = NULL) {
  run_simulation_argcheck(simulation, layer)

  if (is.null(layer))  # Set as most downstream layer
    layer <- erahumed_layers() [[ length(erahumed_layers()) ]]

  for (l in c(upstream_layers(layer), layer)) {
    simulation <- compute_layer(simulation, l)
  }

  return(simulation)
}

run_simulation_argcheck <- function(simulation, layer) {
  tryCatch(
    {
      assert_erahumed_simulation(simulation)

      if (!is.null(layer)) {
        assert_string(layer)
        if (!(layer %in% erahumed_layers()))
          stop("Invalid 'layer' argument.")
      }
    },
    error = function(e) {
      class(e) <- c("run_simulation_argcheck_error", class(e))
      stop(e)
    }
  )
}
