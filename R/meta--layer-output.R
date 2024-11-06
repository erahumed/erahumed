#' @title Extract model layer outputs
#'
#' @author Valerio Gherardi
#'
#' @description
#' Extracts the output `data.frame` of a \link{erahumed_simulation_layer}.
#'
#' @param object The object from which the layer output is to be extracted
#' from (see above for available methods).
#' @param layer A string. Name of the layer whose output `data.frame`
#' is to be extracted.
#' @param ... further arguments passed to or from other methods.
#'
#' @return
#' A `data.frame`.
#'
#' @export
layer_output <- function(object, ...) {
  UseMethod("layer_output", object)
}

#' @rdname layer_output
#' @export
layer_output.erahumed_simulation_layer <- function(object, ...) {
  return(object$output)
}

#' @rdname layer_output
#' @export
layer_output.erahumed_simulation <- function(object, layer, ...) {
  comp_obj <- get_simulation_layer(object, layer)
  layer_output(comp_obj)
}

