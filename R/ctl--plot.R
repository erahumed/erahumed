#' Plot ctl simulation layer output
#' @noRd
plot.erahumed_ctl <- function(
    x,
    type = c("lake_view"),
    variable = c("mass", "density"),
    chemicals = NULL,
    dygraph_group = NULL,
    ...
)
{
  switch(match.arg(type),
         lake_view = plot_ctl_lake_view(x,
                                        variable = match.arg(variable),
                                        chemicals = chemicals,
                                        dygraph_group = dygraph_group,
                                        ...)
         )
}

plot_ctl_lake_view <- function(x, variable, chemicals, dygraph_group, ...) {
  ct_plot_time_series(ct_output_df = get_layer_output(x),
                      variable = variable,
                      chemicals = chemicals,
                      dygraph_group = dygraph_group)
}

