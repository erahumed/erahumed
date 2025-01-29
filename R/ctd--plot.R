#' Plot ctd simulation layer output
#' @noRd
plot.erahumed_ctd <- function(
    x,
    type = c("ditch_view", "max_boxplot"),
    variable = c("mass", "density"),
    ...
)
{
  switch(match.arg(type),
         cluster_view = plot_ctd_ditch_view(x, variable = variable, ...),
         max_boxplot = plot_ctd_max_boxplot(x, ...)
         )
}

plot_ctd_ditch_view <- function(x, variable = c("mass", "density"), ...) {
  variable <- match.arg(variable)
  ct_output_df <- get_layer_output(x)

  args <- list(...)
  ditch <- args$ditch
  if (is.null(ditch)) {
    ditch <- ct_output_df$ditch[[1]]
    warning(paste0(
      "No cluster specified through the 'cluster_id' argument. ",
      "Plotting cluster '", ditch, "'."
    ))
  }

  ct_output_df <- ct_output_df |>
    (\(.) .[.$ditch == ditch, ])()

  switch(variable,
         mass = ct_plot_mass_time_series(ct_output_df),
         density = ct_plot_density_time_series(ct_output_df)
  )
}

plot_ctd_max_boxplot <- function(x, ...) {
  density_variables <- c("cs", "cw", "cw_outflow")

  get_layer_output(x) |>
    # (\(.) .[.$chemical == chemical, ])() |>
    stats::aggregate(
      by = cbind(cs, cw, cw_outflow) ~ chemical + ditch,
      FUN = max,
      na.rm = TRUE
    ) |>
    stats::reshape(
      varying = list(density_variables),
      v.names = "value",
      timevar = "variable",
      times = density_variables,
      idvar = c("ditch", "chemical"),
      direction = "long"
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$variable, y = .data$value)) +
    ggplot2::geom_violin() +
    ggplot2::geom_boxplot(alpha = 0.5) +
    ggplot2::facet_wrap("chemical", scales = "free_y") +
    ggplot2::xlab(NULL) +
    ggplot2::ylab("Density [Kg / m\u{00B3}]") +
    NULL
}
