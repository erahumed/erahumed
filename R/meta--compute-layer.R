compute_layer <- function(simulation, layer = erahumed_layers())
{
  assert_erahumed_simulation(simulation)
  layer <- match.arg(layer)
  check_upstream_layers(simulation, layer)

  if ( !is.null(get_layer_output(simulation, layer)) )
    return(simulation)

  compute_fun <- switch(layer,
    inp = compute_inp,
    hba = compute_hba,
    hbp = compute_hbp,
    ca = compute_ca,
    ct = compute_ct
  )

  simulation <- compute_fun(simulation)

  return(simulation)
}



check_upstream_layers <- function(simulation, layer) {
  tryCatch({
    for (upstream_layer in upstream_layers(layer)) {
      if (!is.null( get_layer_output(simulation, upstream_layer) )) next

      msg <- paste0(
        "Upstream layer '", upstream_layer, "' of model must be computed first."
      )
      stop(msg)
    }
  },
  error = function(e) {
    class(e) <- c("check_upstream_layers_error", class(e))
    stop(e)
  })
}
