#' @title Extract model layer parameters
#'
#' @author Valerio Gherardi
#'
#' @description
#' Extracts the parameters used in the computation of a model layer.
#'
#' @param object The object from which the layer parameters are to be
#' extracted from (see above for available methods).
#' @param layer A string. Name of the layer whose parameters `list`
#' is to be extracted.
#' @param ... further arguments passed to or from other methods.
#'
#' @return
#' A `list`.
#'
#' @export
layer_parameters <- function(object, ...) {
  UseMethod("layer_parameters", object)
}

#' @rdname layer_parameters
#' @export
layer_parameters.erahumed_simulation_layer <- function(object, ...) {
  return(object$params)
}

#' @rdname layer_parameters
#' @export
layer_parameters.erahumed_simulation <- function(object, layer, ...) {
  comp_obj <- get_simulation_layer(object, layer)
  layer_parameters(comp_obj)
}

