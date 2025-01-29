#' Plot ctc simulation layer output
#'
#' @description
#' Plot method for \link{ctc} simulation layers.
#'
#' @param x An object of class `ctc`.
#' @param type Type of plot to be generated. Currently, only the
#' `"cluster_view"` plot type is implemented.
#' @param variable Variable to plot for `"cluster_view"` type plots, either
#' `"mass"` or `"density"`; ignored for other types of plots.
#' @param ... Further plotting parameters, associated with the plot type
#' specified by `type`; See details.
#' @param ... Further plotting parameters.
#'
#' @details
#' The `"cluster_view"` plot produces a time series plot of the daily
#' total amounts or densities of chemical (in Kg) for an individual cluster.
#' In order to use this plotting method, the user must provide an additional
#' `cluster_id` argument, a string specifying the identifier of the cluster
#' whose levels are to be plotted.
#'
#' The `"max_boxplot"` type plot produces a box plot of the maximum chemical
#' densities (in Kg per cube meter) present in sediment, water and outflow.
#'
#' @return A \link[dygraphs]{dygraph} plot.
#'
#' @noRd
plot.erahumed_ctc <- function(
    x,
    type = c("cluster_view", "max_boxplot"),
    variable = c("mass", "density"),
    ...
    )
{
  switch(match.arg(type),
         cluster_view = plot_ctc_cluster_view(x, variable = variable, ...),
         max_boxplot = plot_ctc_max_boxplot(x, ...)
         )
}

plot_ctc_cluster_view <- function(x, variable = c("mass", "density"), ...) {
  variable <- match.arg(variable)
  ct_output_df <- get_layer_output(x)

  args <- list(...)
  cluster_id <- args$cluster_id
  if (is.null(cluster_id)) {
    cluster_id <- ct_output_df$cluster_id[[1]]
    warning(paste0(
      "No cluster specified through the 'cluster_id' argument. ",
      "Plotting cluster '", cluster_id, "'."
    ))
  }

  ct_output_df <- ct_output_df |>
    (\(.) .[.$cluster_id == cluster_id, ])()

  switch(variable,
         mass = ct_plot_mass_time_series(ct_output_df),
         density = ct_plot_density_time_series(ct_output_df)
         )
}

plot_ctc_max_boxplot <- function(x, ...) {
  density_variables <- c("cs", "cw", "cw_outflow")

  get_layer_output(x) |>
    stats::aggregate(
      by = cbind(cs, cw, cw_outflow) ~ chemical + cluster_id,
      FUN = max,
      na.rm = TRUE
    ) |>
    stats::reshape(
      varying = list(density_variables),
      v.names = "value",
      timevar = "variable",
      times = density_variables,
      idvar = c("cluster_id", "chemical"),
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
