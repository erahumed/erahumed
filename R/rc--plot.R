plot_rc <- function(simulation,
                    type = c("chronic", "acute"),
                    element_id = NULL,
                    dygraph_group = NULL,
                    ...)
{
  assert_erahumed_simulation(simulation)
  type <- match.arg(type)

  r_output <- get_output(simulation, "rc")

  if (is.null(element_id)) {
    element_id <- r_output$element_id[[1]]
    warning(paste0(
      "No cluster specified through the 'element_id' argument. ",
      "Plotting cluster '", element_id, "'."
    ))
  }

  r_output <- r_output |>
    (\(.) .[.$element_id == element_id, ])()

  plot_risk(r_output, type = type, dygraph_group = dygraph_group)
}
