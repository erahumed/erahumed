plot_rl <- function(simulation,
                    type = c("chronic", "acute"),
                    dygraph_group = NULL,
                    ...)
{
  assert_erahumed_simulation(simulation)
  type <- match.arg(type)

  r_output <- get_output(simulation, "rl")

  plot_risk(r_output,
            type = type,
            dygraph_group = dygraph_group,
            chemical_db = get_etc(simulation, "chemical_db")
            )
}
