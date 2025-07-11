plot_ctd <- function(
    simulation,
    element_id = NULL,
    compartment = c("water", "sediment"),
    chemical_ids = NULL,
    dygraph_group = NULL,
    ...
)
{
  assert_erahumed_simulation(simulation)
  compartment <- match.arg(compartment)

  data <- get_output(simulation, "ctd")

  if (is.null(element_id)) {
    element_id <- data$element_id[[1]]
    warning(paste0(
      "No ditch specified through the 'element_id' argument. ",
      "Plotting ditch '", element_id, "'."
    ))
  }

  data <- data[data$element_id == element_id, ]

  ct_plot_time_series_density(data,
                              compartment = compartment,
                              chemical_ids = chemical_ids,
                              dygraph_group = dygraph_group,
                              chemical_db = get_etc(simulation, "chemical_db")
                              )
}
