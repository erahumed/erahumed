plot_ctl <- function(
    simulation,
    compartment = c("water", "sediment"),
    chemical_ids = NULL,
    dygraph_group = NULL,
    ...
)
{
  assert_erahumed_simulation(simulation)
  compartment <- match.arg(compartment)

  data <- get_output(simulation, "ctl")

  ct_plot_time_series_density(data,
                              compartment = compartment,
                              chemical_ids = chemical_ids,
                              dygraph_group = dygraph_group,
                              chemical_db = get_etc(simulation, "chemical_db")
                              )
}
