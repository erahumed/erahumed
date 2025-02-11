#' @noRd
plot.erahumed_rd <- function(x,
                             type = c("chronic", "acute"),
                             ditch = NULL,
                             dygraph_group = NULL,
                             ...)
{
  type <- match.arg(type)
  r_output <- get_layer_output(x)

  if (is.null(ditch)) {
    ditch <- r_output$element_id[[1]]
    warning(paste0(
      "No cluster specified through the 'ditch' argument. ",
      "Plotting ditch '", ditch, "'."
    ))
  }

  r_output <- r_output |>
    (\(.) .[.$element_id == ditch, ])()

  plot_risk(r_output, type = type, dygraph_group = dygraph_group)
}
