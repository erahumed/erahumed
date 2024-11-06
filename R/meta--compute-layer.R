compute_layer <- function(model, layer = erahumed_layers(), ...)
{
  compute_layer_basecheck(model, layer)
  layer <- match.arg(layer)

  compute_argcheck <- get_layer_argcheck_fun(layer)
  compute_output <- get_layer_output_fun(layer)
  new_layer <- get_layer_constructor_fun(layer)

  compute_argcheck(...)
  output <- compute_output(model, ...)
  params <- list(...)

  model[[layer]] <- new_layer(output, params)

  for (comp in downstream_layers(layer))
    model[[comp]] <- NULL

  return(model)
}



upstream_layers <- function(layer) {
  i <- match(layer, erahumed_layers())

  erahumed_layers() |> (\(.) .[seq_along(.) < i])()
}



downstream_layers <- function(layer) {
  i <- match(layer, erahumed_layers())

  erahumed_layers() |> (\(.) .[seq_along(.) > i])()
}



compute_layer_basecheck <- function(model,
                                        layer = erahumed_layers()
                                        )
{
  tryCatch({
    assert_erahumed_simulation(model)
    match.arg(layer)

    for (comp in upstream_layers(layer)) {
      if (!is.null(model[[comp]])) next

      msg <- paste0(
        "Upstream layer '", comp, "' of simulation must be computed first.")
      stop(msg)
    }
  },
  error = function(e) {
    class(e) <- c("compute_layer_basecheck_error", class(e))
    stop(e)
  })
}



get_layer_argcheck_fun <- function(layer) {
  res <- get_erahumed_fun( paste0("compute_", layer, "_argcheck") )
  if (is.null(res))
    res <- function(...) return(TRUE)
  return(res)
}



get_layer_output_fun <- function(layer) {
  res <- get_erahumed_fun( paste0("compute_", layer, "_output") )
  if (is.null(res))
    stop(paste0("Model layer, '", layer, "' not yet implemented."))
  return(res)
}



get_layer_output_validator_fun <- function(layer) {
  res <- get_erahumed_fun( paste0(layer, "_validate_output") )
  if (is.null(res))
    res <- function(...) return(assert_data.frame)
  return(res)
}



get_layer_constructor_fun <- function(layer) {
  validate_output <- get_layer_output_validator_fun(layer)
  base_class <- paste0("erahumed_", layer)

  function(output, params) {
    res <- new_simulation_layer(output,
                               params,
                               validate_output = validate_output
                               )
    class(res) <- c(base_class, class(res))
    return(res)
  }
}



get_erahumed_fun <- function(fun_name) {
  tryCatch(
    get(fun_name, envir = asNamespace("erahumed")),
    error = function(e) return(NULL)
  )
}
