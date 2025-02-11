#' Plot HBD simulation layer output
#'
#' @description
#' Plot method for \link{hbd} simulation layers.
#'
#' @param x An object of class `hbd`.
#' @param ditch string. Ditch to be plotted, one of the values of the `ditch `
#' column in the `info_ditches()` output.
#' @param ... Not used
#'
#' @return A \link[dygraphs]{dygraph} plot.
#'
#' @noRd
plot.erahumed_hbd <- function(x, ditch = NULL, dygraph_group = NULL, ...) {

  data <- get_layer_output(x)

  if (is.null(ditch)) {
    ditch <- data$ditch[[1]]
    warning(paste0(
      "No ditch specified through the 'ditch' argument. ",
      "Plotting ditch '", ditch, "'."
    ))
  }


  data_ditch <- data[
    data$ditch == ditch, c("date", "outflow_lake_m3", "inflow_clusters_m3")
    ]

  dygraphs::dygraph(data_ditch,
                    main = paste("Time Series for ditch", ditch),
                    group = dygraph_group
                    ) |>
    dygraphs::dySeries(name = "outflow_lake_m3",
                       label = "Total Inflow",
                       color = "black") |>
    dygraphs::dySeries(name = "inflow_clusters_m3",
                       label = "Clusters Inflow",
                       color = "blue") |>
    dygraphs::dyLegend(show = "always") |>
    dygraphs::dyOptions(axisLabelWidth = 80) |>  # Adjust to prevent overlap
    dygraphs::dyRangeSelector() |>
    dygraphs::dyUnzoom()
}
