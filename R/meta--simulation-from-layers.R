simulation_from_layers <- function(inp = NULL,
                                   hbl = NULL,
                                   hbc = NULL,
                                   hbd = NULL,
                                   ca = NULL,
                                   ct = NULL)
{
  s <- erahumed_simulation() |>
    sim_replace_layer("inp", inp) |>
    sim_replace_layer("hbl", hbl) |>
    sim_replace_layer("hbc", hbc) |>
    sim_replace_layer("hbd", hbd) |>
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
