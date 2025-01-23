#' Initialize an ERAHUMED simulation
#'
#' @description
#' Initializes an ERAHUMED simulation. Check the
#' [main package vignette](https://erahumed.github.io/erahumed/articles/erahumed-workflow.html)
#' for a detailed description of the simulation workflow.
#'
#' @return An object of class `erahumed_simulation`.
#'
#' @examples
#' erahumed_simulation()
#'
#' @export
erahumed_simulation <- function()
{

  structure(list(), class = "erahumed_simulation") |>
    setup_hydrology() |>
    setup_exposure() |>
    setup_risk()
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
print.erahumed_simulation <- function(x, ...) {
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
