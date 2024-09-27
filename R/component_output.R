#' @title Extract model component outputs
#'
#' @author Valerio Gherardi
#'
#' @description
#' Extracts the output `data.frame` of a \link{erahumed_model_component}.
#'
#' @param component An object inheriting from class
#' \link{erahumed_model_component}.
#'
#' @return
#' A `data.frame`.
#'
#' @export
component_output <- function(object, ...) {
  UseMethod("component_output", object)
}

#' @export
component_output.erahumed_model_component <- function(object, ...) {
  return(object$output)
}

#' @export
component_output.erahumed_model <- function(object, component, ...) {
  comp_obj <- get_model_component(object, component)
  return(comp_obj$output)
}

