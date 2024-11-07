#' Initialize an ERAHUMED simulation
#'
#' @description
#' Initializes an ERAHUMED simulation. See \link{erahumed_simulation_interface}
#' for a detailed description of the simulation workflow.
#'
#' @return An object of class `erahumed_simulation`.
#'
#' @author Valerio Gherardi
#'
#' @examples
#' erahumed_simulation()
#'
#' @export
erahumed_simulation <- function()
  new_erahumed_simulation()

new_erahumed_simulation <- function() {
  structure(list(), class = "erahumed_simulation") |>
    setup_inp() |>
    setup_hba() |>
    setup_hbp() |>
    setup_ca() |>
    setup_ct()
}


is_erahumed_simulation <- function(obj) {
  if (!is.list(obj))
    return(FALSE)

  elems_have_right_class <- sapply(obj, inherits, "erahumed_simulation_layer")
  if (!all(elems_have_right_class))
    return(FALSE)

  if ( !inherits(obj, "erahumed_simulation") )
    return(FALSE)

  return(TRUE)
}

#' @export
print.erahumed_simulation <- function(x, ..., max = 100) {
  cat("An ERAHUMED simulation.")

  layer_is_computed <- sapply(erahumed_layers(), \(layer) {
    !is.null(get_layer_output(x, layer))
  })
  if (sum(layer_is_computed) == 0)
    computed_layers <- "None"
  else
    computed_layers <- paste(erahumed_layers()[layer_is_computed],
                             collapse = ", ")

  cat("\nComputed layers:", computed_layers)

  return(invisible(x))
}

#' @export
summary.erahumed_simulation <- function(object, ...) {
  print(object)
}
