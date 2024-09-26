erahumed_components <- function()
  c("inp", "hba", "hbp", "ca", "ct")

compute_component <- function(
    model,
    component = erahumed_components(),
    ...
    )
{
  assert_erahumed_model(model)
  component <- match.arg(component)

  compute_argcheck <- get_component_argcheck_fun(component)
  compute_output <- get_component_output_fun(component)
  new_component <- get_component_constructor_fun(component)

  compute_argcheck(...)
  output <- compute_output(model, ...)
  params <- list(...)

  model[[component]] <- new_component(output, params)

  component_level <- match(component, erahumed_components())
  downstream_components <- erahumed_components()[ -(1:component_level) ]
  for (c in downstream_components)
    model[[c]] <- NULL

  return(model)
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
  res <-   get_erahumed_fun( paste0(component, "_validate_output") )
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
