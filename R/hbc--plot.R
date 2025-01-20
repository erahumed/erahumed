#' Plot hbc simulation layer output
#'
#' @description
#' Plot method for \link{hbc} simulation layers.
#'
#' @param x An object of class `hbc`.
#' @param type Type of plot to be generated. Currently, only the
#' `"cluster_view"` plot type is implemented.
#' @param ... Further plotting parameters, associated with the plot type
#' specified by `type`; See details.
#'
#' @details
#' The `"cluster_view"` plot generates a time series plot of the daily
#' water levels (in cm) of an individual cluster. In order to use this plotting
#' method, the user must provide an additional `cluster_id` argument, a string
#' specifying the identifier of the cluster whose levels are to be plotted.
#'
#' @return A \link[dygraphs]{dygraph} plot.
#'
#' @export
plot.erahumed_hbc <- function(x, type = c("cluster_view", "map_view"), ...) {
  type <- match.arg(type)

  switch(type,
         cluster_view = plot_erahumed_hbc_cluster_view(x, ...),
         map_view = plot_erahumed_hbc_map_view(x, ...)
  )
}

plot_erahumed_hbc_cluster_view <- function(x, ...) {
  args <- list(...)
  data <- get_layer_output(x)

  cluster_id <- args$cluster_id

  if (is.null(cluster_id)) {
    cluster_id <- data$cluster_id[[1]]
    warning(paste0(
      "No cluster specified through the 'cluster_id' argument. ",
      "Plotting cluster '", cluster_id, "'."
      ))
  }


  data_cluster <- data[data$cluster_id == cluster_id, ]

  # Prepare time series data
  data_to_plot <- data_cluster[, c("date", "ideal_height_eod_cm", "height_sod_cm",
                                   "inflow_cm", "outflow_cm", "petp_cm")]

  # Adjust for outflows (negative values)
  data_to_plot$outflow_cm <- -data_to_plot$outflow_cm

  # Rename columns for clarity
  colnames(data_to_plot) <- c("Date", "Ideal Height", "Simulated Height",
                              "Inflow", "Outflow", "P-ETP")

  # Create dygraph
  dygraphs::dygraph(data_to_plot, main = paste("Time Series for Cluster", cluster_id)) |>
    dygraphs::dySeries("Ideal Height", color = "black", strokePattern = "dashed", strokeWidth = 2) |>
    dygraphs::dySeries("Simulated Height", color = "black", strokePattern = "solid", strokeWidth = 2) |>
    dygraphs::dySeries("Inflow", stepPlot = TRUE, color = "red") |>
    dygraphs::dySeries("Outflow", stepPlot = TRUE, color = "red") |>
    dygraphs::dySeries("P-ETP", stepPlot = TRUE, color = "blue") |>
    dygraphs::dyAxis("y", label = "Water Level / Flow [cm]", independentTicks = TRUE) |>
    dygraphs::dyLegend(show = "always", width = 800) |>
    dygraphs::dyOptions(axisLabelWidth = 80) |>  # Adjust to prevent overlap
    dygraphs::dyRangeSelector() |>
    dygraphs::dyUnzoom()
}

plot_erahumed_hbc_map_view <- function(x, ...) {
  stop("Not yet implemented.")
}
