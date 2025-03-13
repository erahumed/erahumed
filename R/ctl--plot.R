plot_ctl <- function(
    simulation,
    compartment = c("water", "sediment"),
    chemicals = NULL,
    dygraph_group = NULL,
    ...
)
{
  assert_erahumed_simulation(simulation)
  compartment <- match.arg(compartment)

  data <- get_output(simulation, "ctl")

  ct_plot_time_series_density(data,
                              compartment = compartment,
                              chemicals = chemicals,
                              dygraph_group = dygraph_group)
}
