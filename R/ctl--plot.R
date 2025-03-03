#' Plot ctl simulation layer output
#' @noRd
plot.erahumed_ctl <- function(
    x,
    compartment = c("water", "sediment"),
    chemicals = NULL,
    dygraph_group = NULL,
    ...
)
{
  compartment <- match.arg(compartment)

  data <- get_layer_output(x)

  ct_plot_time_series_density(data,
                              compartment = compartment,
                              chemicals = chemicals,
                              dygraph_group = dygraph_group)
}
