#' @noRd
plot.erahumed_rd <- function(x,
                             type = c("chronic", "acute"),
                             element_id = NULL,
                             dygraph_group = NULL,
                             ...)
{
  type <- match.arg(type)
  r_output <- get_layer_output(x)

  if (is.null(element_id)) {
    element_id <- r_output$element_id[[1]]
    warning(paste0(
      "No cluster specified through the 'element_id' argument. ",
      "Plotting ditch '", element_id, "'."
    ))
  }

  r_output <- r_output |>
    (\(.) .[.$element_id == element_id, ])()

  plot_risk(r_output, type = type, dygraph_group = dygraph_group)
}
