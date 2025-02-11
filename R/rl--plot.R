#' @noRd
plot.erahumed_rl <- function(x,
                             type = c("chronic", "acute"),
                             dygraph_group = NULL,
                             ...)
{
  type <- match.arg(type)
  r_output <- get_layer_output(x)

  plot_risk(r_output, type = type, dygraph_group = dygraph_group)
}
