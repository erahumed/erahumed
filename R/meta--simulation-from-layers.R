simulation_from_layers <- function(inp = NULL,
                                   hba = NULL,
                                   hbp = NULL,
                                   ca = NULL,
                                   ct = NULL)
{
  s <- erahumed_simulation() |>
    sim_replace_layer("inp", inp) |>
    sim_replace_layer("hba", hba) |>
    sim_replace_layer("hbp", hbp) |>
    sim_replace_layer("ca", ca) |>
    sim_replace_layer("ct", ct)

  assert_erahumed_simulation(s)

  return(s)
}

sim_replace_layer <- function(simulation, layer, replacement) {
  if (!is.null(replacement))
    simulation [[layer]] <- replacement
  return(simulation)
}
