#' @title Extract model component parameters
#'
#' @author Valerio Gherardi
#'
#' @description
#' Extracts the parameters used in the computation of a model component.
#'
#' @param object The object from which the component parameters are to be
#' extracted from (see above for available methods).
#' @param component A string. Name of the component whose parameters `list`
#' is to be extracted.
#' @param ... further arguments passed to or from other methods.
#'
#' @return
#' A `list`.
#'
#' @export
component_parameters <- function(object, ...) {
  UseMethod("component_parameters", object)
}

#' @rdname component_parameters
#' @export
component_parameters.erahumed_model_component <- function(object, ...) {
  return(object$params)
}

#' @rdname component_output
#' @export
component_parameters.erahumed_model <- function(object, component, ...) {
  comp_obj <- get_model_component(object, component)
  component_parameters(comp_obj)
}

