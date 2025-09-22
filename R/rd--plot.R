plot_rd <- function(simulation,
                    type = c("chronic", "acute"),
                    method = c("paf", "rq"),
                    element_id = NULL,
                    dygraph_group = NULL,
                    chemical_ids = NULL,
                    ...)
{
  assert_erahumed_simulation(simulation)
  type <- match.arg(type)
  method <- match.arg(method)

  r_output <- get_raw_output(simulation, "rd")

  if (is.null(element_id)) {
    element_id <- r_output$element_id[[1]]
    warning(paste0(
      "No cluster specified through the 'element_id' argument. ",
      "Plotting ditch '", element_id, "'."
    ))
  }

  r_output <- r_output |>
    (\(.) .[.$element_id == element_id, ])()

  plot_risk(r_output,
            type = type,
            method = method,
            dygraph_group = dygraph_group,
            chemical_db = get_etc(simulation, "chemical_db"),
            chemical_ids = chemical_ids
            )
}
