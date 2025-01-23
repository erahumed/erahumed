#' Run an ERAHUMED simulation
#'
#' @description
#' Computes the layers of a ERAHUMED simulation. Check the
#' [main package vignette](https://erahumed.github.io/erahumed/articles/erahumed-workflow.html)
#' for a detailed description of the simulation workflow.
#'
#' @param simulation `[`\link{erahumed_simulation}`]` \cr
#' The simulation object containing the layers to be computed.
#'
#' @return An \link{erahumed_simulation}.
#'
#' @export
run_simulation <- function(simulation)
{
  assert_erahumed_simulation(simulation)

  simulation |>
    compute_hydrology() |>
    compute_exposure() |>
    compute_risk()
}
