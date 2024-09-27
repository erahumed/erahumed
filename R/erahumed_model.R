#' Initialize an ERAHUMED model
#'
#' @description
#' Initializes an ERAHUMED model. This is the starting point for the ERAHUMED
#' model chain, whose computations are performed by `compute_*()` functions.
#' See \link{erahumed_modeling_interface} for more details.
#'
#' @return An object of class `erahumed_model`, that represents a blank ERAHUMED
#' model (with no model component computed yet).
#'
#' @author Valerio Gherardi
#'
#' @examples
#' m <- erahumed_model()
#' m
#'
#' @export
erahumed_model <- function()
  new_erahumed_model()

new_erahumed_model <- function()
  structure(list(), class = "erahumed_model")

is_erahumed_model <- function(obj) {
  if (!is.list(obj))
    return(FALSE)

  elems_have_right_class <- sapply(obj, inherits, "erahumed_model_component")
  if (!all(elems_have_right_class))
    return(FALSE)

  if ( !inherits(obj, class(new_erahumed_model())) )
    return(FALSE)

  return(TRUE)
}

#' @export
print.erahumed_model <- function(x, ..., max = 100) {
  cat(bold("An ERAHUMED model."))

  comps <- if (length(x) == 0) "None" else paste(names(x), collapse = ", ")
  cat("\nCalculated components: ", comps)
}

#' @export
print.erahumed_model <- function(x, ...) {
  cat(bold("An ERAHUMED model."))

  comps <- if (length(x) == 0) "None" else paste(names(x), collapse = ", ")
  cat("\n\nCalculated components:", comps)
}

#' @export
summary.erahumed_model <- function(object, ...) {
  print(object)
}
