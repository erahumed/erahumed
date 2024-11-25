compute_layer <- function(simulation, layer = erahumed_layers())
{
  assert_erahumed_simulation(simulation)
  layer <- match.arg(layer)

  if (!is.null(simulation [[layer]] [["output"]]))
    return(simulation)

  check_upstream_layers(simulation, layer)

  compute_bare_fun <- get_compute_bare_fun(layer)
  validate_output <- get_validate_output_fun(layer)

  output <- compute_bare_fun(simulation)
  validate_output(output)

  simulation [[layer]] [["output"]] <- output

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



get_erahumed_fun <- function(fun_name) {
  tryCatch(
    get(fun_name, envir = asNamespace("erahumed")),
    error = function(e) return(NULL)
  )
}

get_validate_output_fun <- function(layer) {
  res <- get_erahumed_fun( paste("validate", layer, "output", sep = "_") )
  if (is.null(res))
    res <- function(...) return(assert_data.frame)
  return(res)
}

get_compute_bare_fun <- function(layer) {
  res <- get_erahumed_fun( paste("compute", layer, "bare", sep = "_") )
  if (is.null(res))
    stop(paste0("Model layer, '", layer, "' not yet implemented."))
  return(res)
}
