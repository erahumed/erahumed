#' Plot ctl simulation layer output
#' @noRd
plot.erahumed_ctl <- function(
    x,
    type = c("lake_view"),
    variable = c("mass", "density"),
    ...
)
{
  switch(match.arg(type),
         lake_view = plot_ctl_lake_view(x, variable = variable, ...)
         )
}

plot_ctl_lake_view <- function(x, variable = c("mass", "density"), ...) {
  variable <- match.arg(variable)
  ct_output_df <- get_layer_output(x)

  switch(variable,
         mass = ct_plot_mass_time_series(ct_output_df),
         density = ct_plot_density_time_series(ct_output_df)
         )
}

