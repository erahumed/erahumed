#' @rdname erahumed_parameters
#'
#' @export
setup_risk <- function(simulation)
{
  simulation |>
    setup_rc() |>
    setup_rd() |>
    setup_rl()
}

compute_risk <- function(simulation) {
  assert_erahumed_simulation(simulation)

  simulation |>
    compute_rc() |>
    compute_rd() |>
    compute_rl()
}

extract_risk <- function(simulation, element = c("lake", "ditch", "cluster"))
{
  assert_erahumed_simulation(simulation)
  element <- match.arg(element)

  switch(element,
         lake = get_layer_output(simulation, "rl"),
         ditch = get_layer_output(simulation, "rd"),
         cluster =  get_layer_output(simulation, "rc")
         )
}
