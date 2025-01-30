#' Plot ctd simulation layer output
#' @noRd
plot.erahumed_ctd <- function(
    x,
    type = c("ditch_view", "max_boxplot"),
    variable = c("mass", "density"),
    chemicals = NULL,
    ...
)
{
  switch(match.arg(type),
         ditch_view = plot_ctd_ditch_view(x,
                                          variable = match.arg(variable),
                                          chemicals = chemicals,
                                          ...),
         max_boxplot = plot_ctd_max_boxplot(x, ...)
         )
}

plot_ctd_ditch_view <- function(x, variable, chemicals, ...) {
  ct_output_df <- get_layer_output(x)

  args <- list(...)
  ditch <- args$ditch
  if (is.null(ditch)) {
    ditch <- ct_output_df$ditch[[1]]
    warning(paste0(
      "No ditch specified through the 'ditch' argument. ",
      "Plotting ditch '", ditch, "'."
    ))
  }

  ct_output_df <- ct_output_df |>
    (\(.) .[.$ditch == ditch, ])()

  ct_plot_time_series(ct_output_df = ct_output_df, variable = variable, chemicals = chemicals)
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
