#' Initialize an ERAHUMED simulation
#'
#' @description
#' Initializes an ERAHUMED simulation. This is the starting point for the ERAHUMED
#' simulation chain, whose computations are performed by `compute_*()` functions.
#' See \link{erahumed_simulation_interface} for more details.
#'
#' @return An object of class `erahumed_simulation`, that represents a blank ERAHUMED
#' simulation (with no simulation layer computed yet).
#'
#' @author Valerio Gherardi
#'
#' @examples
#' m <- erahumed_simulation()
#' m
#'
#' @export
erahumed_simulation <- function()
  new_erahumed_simulation()

new_erahumed_simulation <- function()
  structure(list(), class = "erahumed_simulation")

is_erahumed_simulation <- function(obj) {
  if (!is.list(obj))
    return(FALSE)

  elems_have_right_class <- sapply(obj, inherits, "erahumed_simulation_layer")
  if (!all(elems_have_right_class))
    return(FALSE)

  if ( !inherits(obj, class(new_erahumed_simulation())) )
    return(FALSE)

  return(TRUE)
}

#' @export
print.erahumed_simulation <- function(x, ..., max = 100) {
  cat("An ERAHUMED simulation.")

  comps <- if (length(x) == 0) "None" else paste(names(x), collapse = ", ")
  cat("\nCalculated layers: ", comps)
}

#' @export
summary.erahumed_simulation <- function(object, ...) {
  print(object)
}
