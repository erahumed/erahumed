#' @title Extract simulation results
#'
#' @description Extract ERAHUMED simulation results from an
#' \link{erahumed_simulation} object.
#'
#' @param simulation `[`\link{erahumed_simulation}`]` \cr
#' The simulation object being modified.
#' @param component `[character(1)]` \cr
#' The simulation component to be extracted.
#' Either `"hydrology"`, `"exposure"`, or `"risk"`.
#' @param element `[character(1)]` \cr
#' The landscape element for which simulation results are requested.
#' Either `"lake"`, `"ditch"`, or `"cluster"`.
#'
#' @return A `data.frame`.
#'
#' @export
get_results  <- function(simulation,
                         component = c("hydrology", "exposure", "risk"),
                         element = c("lake", "ditch", "cluster")
                         )
{
  assert_erahumed_simulation(simulation)
  component <- match.arg(component)
  element <- match.arg(element)

  layer <- paste0(
    c(hydrology = "hb", exposure = "ct", risk = "r")[[component]],
    c(lake = "l", ditch = "d", cluster = "c")[[element]]
  )

  get_raw_output(simulation, layer)
}
