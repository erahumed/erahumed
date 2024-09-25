#' @export
erahumed_model <- function()
  new_erahumed_model()

new_erahumed_model <- function() {

  comp <- list_model_components()
  obj <- structure(
    vector("list", length(comp)),
    names = comp,
    class = "erahumed_model"
    )

  return(obj)
}

list_model_components <- function() {
  c("inp", "hba", "hbp", "ca", "ct")
}

is_erahumed_model <- function(obj) {
  if (!is.list(obj))
    return(FALSE)

  if (!identical(names(obj), list_model_components()))
    return(FALSE)

  for (name in names(obj)) {
    if (is.null(obj[[name]])) next
    cl <- paste0("erahumed_", name)
    if ( !inherits(obj[[name]],cl) )
      return(FALSE)
  }

  return(TRUE)
}

update_erahumed_model <- function(model, component, output, params) {
  assert_erahumed_model(model)
  assert_string(component)
  stopifnot(component %in% list_model_components())

  model [[component]] [["output"]] <- output
  model [[component]] [["params"]] <- params

  return(model)
}

get_model_component <- function(model,
                                component,
                                value = c("output", "params")
)
{
  assert_erahumed_model(model)
  assert_string(component)
  stopifnot(component %in% list_model_components())
  value = match.arg(value)

  model [[component]] [[value]]
}
