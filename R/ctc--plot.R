plot_ctc <- function(
    simulation,
    element_id = NULL,
    compartment = c("water", "sediment"),
    chemicals = NULL,
    dygraph_group = NULL,
    ...
    )
{
  assert_erahumed_simulation(simulation)
  compartment <- match.arg(compartment)

  data <- get_output(simulation, "ctc")

  if (is.null(element_id)) {
    element_id <- data$element_id[[1]]
    warning(paste0(
      "No cluster specified through the 'element_id' argument. ",
      "Plotting cluster '", element_id, "'."
    ))
  }

  data <- data[data$element_id == element_id, ]

  ct_plot_time_series_density(data,
                              compartment = compartment,
                              chemicals = chemicals,
                              dygraph_group = dygraph_group)
}

