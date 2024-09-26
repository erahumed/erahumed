erahumed_components <- function() {
  res <- c("inp", "hba", "hbp", "ca")
  c(res, "dum") # Add dummy component with upstream dependence from all others
}


compute_component <- function(model, component = erahumed_components(), ...)
{
  compute_component_basecheck(model, component)
  component <- match.arg(component)

  compute_argcheck <- get_component_argcheck_fun(component)
  compute_output <- get_component_output_fun(component)
  new_component <- get_component_constructor_fun(component)

  compute_argcheck(...)
  output <- compute_output(model, ...)
  params <- list(...)

  model[[component]] <- new_component(output, params)

  for (comp in downstream_components(component))
    model[[comp]] <- NULL

  return(model)
}

upstream_components <- function(component) {
  i <- match(component, erahumed_components())

  erahumed_components() |> (\(.) .[seq_along(.) < i])()
}

downstream_components <- function(component) {
  i <- match(component, erahumed_components())

  erahumed_components() |> (\(.) .[seq_along(.) > i])()
}



compute_component_basecheck <- function(model,
                                        component = erahumed_components()
                                        )
{
  tryCatch({
    assert_erahumed_model(model)
    match.arg(component)

    for (comp in upstream_components(component)) {
      if (!is.null(model[[comp]])) next

      msg <- paste0(
        "Upstream component '", comp, "' of model must be computed first.")
      stop(msg)
    }
  },
  error = function(e) {
    class(e) <- c("compute_component_basecheck_error", class(e))
    stop(e)
  })
}

get_component_argcheck_fun <- function(component) {
  res <- get_erahumed_fun( paste0("compute_", component, "_argcheck") )
  if (is.null(res))
    res <- function(...) return(TRUE)
  return(res)
}

get_component_output_fun <- function(component) {
  res <- get_erahumed_fun( paste0("compute_", component, "_output") )
  if (is.null(res))
    stop(paste0("Model component, '", component, "' not yet implemented."))
  return(res)
}

get_component_output_validator_fun <- function(component) {
  res <- get_erahumed_fun( paste0(component, "_validate_output") )
  if (is.null(res))
    res <- function(...) return(assert_data.frame)
  return(res)
}

get_component_constructor_fun <- function(component) {
  validate_output <- get_component_output_validator_fun(component)
  base_class <- paste0("erahumed_", component)

  function(output, params) {
    res <- new_model_component(output,
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
