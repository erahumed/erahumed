plot_rl <- function(simulation,
                    type = c("chronic", "acute"),
                    method = c("paf", "rq"),
                    dygraph_group = NULL,
                    chemical_ids = NULL,
                    ...)
{
  assert_erahumed_simulation(simulation)
  type <- match.arg(type)
  method <- match.arg(method)

  r_output <- get_raw_output(simulation, "rl")

  plot_risk(r_output,
            type = type,
            method = method,
            dygraph_group = dygraph_group,
            chemical_db = get_etc(simulation, "chemical_db"),
            chemical_ids = chemical_ids
            )
}
