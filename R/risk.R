#' @title Ecological risk in the Albufera Natural Park
#' @name risk
#'
#' @description These functions are used to setup, run and extract simulation
#' results for ecological risk in the Albufera Natural Park system.
#'
#' @inheritParams hydrology
#'
#' @inherit hydrology return
#'
#' @export
setup_risk <- function(simulation)
{
  simulation
}

#' @rdname risk
#' @export
compute_risk <- function(simulation) {
  assert_erahumed_simulation(simulation)

  simulation
}

#' @rdname exposure
#' @export
extract_risk <- function(simulation, element = c("lake", "ditch", "cluster"))
{
  assert_erahumed_simulation(simulation)
  element <- match.arg(element)

  switch(element,
         lake = stop("To be implemented."),  # TODO
         ditch = stop("To be implemented."),  # TODO
         cluster =  stop("To be implemented.")  # TODO
  )
}
