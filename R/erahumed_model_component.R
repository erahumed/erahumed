new_model_component <- function(
    output,
    params,
    output_template = NULL,
    validate_params = function(params) return(TRUE)
    )
{
  new_model_component_argcheck(output, params, output_template, validate_params)
  res <- list(output = output, params = params)
  class(res) <- "erahumed_model_component"
  return(res)
}

new_model_component_argcheck <- function(
    output,
    params,
    output_template,
    validate_params
    )
{
  tryCatch({
    assert_data.frame(output, template = output_template)
    assert_list(params)
    if(!validate_params(params))
      stop("Invalid 'params' passed to new_model_component().")
  },
  error = function(e) {
    class(e) <- c("new_model_component_argcheck_error", class(e))
    stop(e)
  })

}
