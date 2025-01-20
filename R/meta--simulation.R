#' Initialize an ERAHUMED simulation
#'
#' @description
#' Initializes an ERAHUMED simulation. See \link{erahumed_simulation_interface}
#' for a detailed description of the simulation workflow.
#'
#' @param outflows_df `r erahumed_param_roxy("outflows_df", "inp")`
#' @param weather_df `r erahumed_param_roxy("weather_df", "inp")`
#' @param variety_prop `r erahumed_param_roxy("variety_prop", "inp")`
#' @param seed `r erahumed_param_roxy("seed", "inp")`
#'
#' @return An object of class `erahumed_simulation`.
#'
#' @examples
#' erahumed_simulation()
#'
#' @export
erahumed_simulation <- function(
    outflows_df = erahumed::albufera_outflows,
    weather_df = erahumed::albufera_weather,
    variety_prop = c("J.Sendra" = 0.8, "Bomba" = 0.1, "Clearfield" = 0.1),
    seed = 840
    )
{
  structure(list(), class = "erahumed_simulation") |>
    setup_inp(outflows_df = outflows_df,
              weather_df = weather_df,
              variety_prop = variety_prop,
              seed = seed
              ) |>
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
