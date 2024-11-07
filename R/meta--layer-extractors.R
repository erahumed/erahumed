#' @title Extract simulation layers
#'
#' @author Valerio Gherardi
#'
#' @description
#' Extracts an \link{erahumed_simulation_layer}.
#'
#' @param simulation The simulation from which the layer is to be extracted
#' from.
#' @param layer A string. Name of the layer to be extracted.
#'
#' @return
#' A \link{erahumed_simulation_layer}.
#'
#' @export
get_layer <- function(simulation, layer = erahumed_layers()) {
  assert_erahumed_simulation(simulation)
  layer <- match.arg(layer)
  if (layer %in% names(simulation))
    return( simulation[[layer]] )
  return(NULL)
}



#' @title Extract simulation layer outputs
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
get_layer_output <- function(object, ...) {
  UseMethod("get_layer_output", object)
}

#' @rdname get_layer_output
#' @export
get_layer_output.erahumed_simulation_layer <- function(object, ...) {
  return(object[["output"]])
}

#' @rdname get_layer_output
#' @export
get_layer_output.erahumed_simulation <- function(object, layer, ...) {
  layer_obj <- get_layer(object, layer)
  get_layer_output(layer_obj)
}

reset_layer_output <- function(simulation, layer = erahumed_layers())
{
  assert_erahumed_simulation(simulation)
  layer <- match.arg(layer)
  simulation [[layer]] [["output"]] <- NULL
  return(simulation)
}



#' @title Extract simulation layer parameters
#'
#' @author Valerio Gherardi
#'
#' @description
#' Extracts the parameters used in the computation of a simulation layer.
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
get_layer_parameters <- function(object, ...) {
  UseMethod("get_layer_parameters", object)
}

#' @rdname get_layer_parameters
#' @export
get_layer_parameters.erahumed_simulation_layer <- function(object, ...) {
  return(object$params)
}

#' @rdname get_layer_parameters
#' @export
get_layer_parameters.erahumed_simulation <- function(object, layer, ...) {
  comp_obj <- get_layer(object, layer)
  get_layer_parameters(comp_obj)
}
