get_model_component <- function(model, component = erahumed_components()) {
  assert_erahumed_model(model)
  component <- match.arg(component)
  if (component %in% names(model))
    return( model[[component]] )
  return(NULL)
}
