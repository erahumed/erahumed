#' Plot ctl simulation layer output
#' @noRd
plot.erahumed_ctl <- function(
    x,
    type = c("lake_view"),
    variable = c("mass", "density"),
    chemicals = NULL,
    ...
)
{
  switch(match.arg(type),
         lake_view = plot_ctl_lake_view(x,
                                        variable = match.arg(variable),
                                        chemicals = chemicals,
                                        ...)
         )
}

plot_ctl_lake_view <- function(x, variable, chemicals, ...) {
  ct_plot_time_series(ct_output_df = get_layer_output(x), variable = variable, chemicals = chemicals)
}

