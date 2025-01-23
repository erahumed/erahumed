#' @rdname erahumed_parameters
#'
#' @export
setup_risk <- function(simulation)
{
  simulation
}

compute_risk <- function(simulation) {
  assert_erahumed_simulation(simulation)

  simulation
}

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
