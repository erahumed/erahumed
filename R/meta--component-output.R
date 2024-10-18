#' @title Extract model component outputs
#'
#' @author Valerio Gherardi
#'
#' @description
#' Extracts the output `data.frame` of a \link{erahumed_model_component}.
#'
#' @param object The object from which the component output is to be extracted
#' from (see above for available methods).
#' @param component A string. Name of the component whose output `data.frame`
#' is to be extracted.
#' @param ... further arguments passed to or from other methods.
#'
#' @return
#' A `data.frame`.
#'
#' @export
component_output <- function(object, ...) {
  UseMethod("component_output", object)
}

#' @rdname component_output
#' @export
component_output.erahumed_model_component <- function(object, ...) {
  return(object$output)
}

#' @rdname component_output
#' @export
component_output.erahumed_model <- function(object, component, ...) {
  comp_obj <- get_model_component(object, component)
  component_output(comp_obj)
}

