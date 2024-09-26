new_model_component <- function(
    output,
    params,
    validate_output = is.data.frame,
    validate_params = is.list
    )
{
  new_model_component_argcheck(output, params, validate_output, validate_params)

  res <- list(output = output, params = params)
  class(res) <- "erahumed_model_component"

  return(res)
}

new_model_component_argcheck <- function(
    output,
    params,
    validate_output,
    validate_params
    )
{
  assert_data.frame(output)
  assert_list(params)

  tryCatch({
    if(!validate_output(output))
      stop("Invalid 'output' passed to new_model_component().")
    if(!validate_params(params))
      stop("Invalid 'params' passed to new_model_component().")
  },
  error = function(e) {
    class(e) <- c("new_model_component_argcheck_error", class(e))
    stop(e)
  })

}

get_model_component <- function(model, component) {
  assert_erahumed_model(model)
  if (component %in% names(model))
    return( model[[component]] )
  return(NULL)
}
