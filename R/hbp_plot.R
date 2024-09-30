#' Plotting local hydrological balance data
#'
#' @description
#' Plot method for local Hydrological balance data, generated through
#' \link{hbp}. A simple wrapper around \link[plotly]{plot_ly} to generate
#' time series plots of the calculated quantities.
#'
#' @param x The object of class `hba` containing the data to be plotted.
#' @param type Type of plot to be generated. Currently, only the
#' `"cluster_levels"` plot is implemented.
#' @param ... Further plotting parameters, associated with the plot type
#' specified by `type`; See details.
#'
#' @details
#' The `"cluster_levels"` plot generates a time series plot of the daily
#' water levels (in cm) of an individual cluster. In order to use this plotting
#' method, the user must provide an additional `cluster_id` argument, a string
#' specifying the identifier of the cluster whose levels are to be plotted (see
#' example below).
#'
#' @return A plotly plot.
#'
#' @export
plot.erahumed_hbp <- function(x, type = c("cluster_levels", "map"), ...) {
  type <- match.arg(type)

  if (type == "map")
    stop("'map' plot not yet implemented.")

  args <- list(...)

  data <- x$output

  if (type == "cluster_levels") {
    if ( !("cluster_id" %in% names(args)) )
      stop("Please specify cluster to plot through the 'cluster_id' argument.")
    return( plot_hbp_cluster_levels(data = data, cluster_id = args$cluster_id) )
  }
}

plot_hbp_cluster_levels <- function(data, cluster_id)
{
  data_cluster <- data[data$cluster_id == cluster_id, ]

  ditch <- data_cluster$ditch[1]
  tancat <- data_cluster$tancat[1]
  variety <- data_cluster$variety[1]

  data_ditch <- data[
    data$ditch == ditch & data$tancat == tancat & data$variety == variety,
    ] |>
    stats::aggregate(real_height_cm ~ date, data = _, FUN = mean)


  plotly::plot_ly(x = ~date) |>
    plotly::add_trace(
      data = data_cluster,
      y = ~ideal_height_cm,
      hoverinfo = "skip",
      type = "scatter",
      mode = "lines",
      line = list(width = 1.5, color = "#0000BB", dash = "dash"),
      name = "Ideal"
    ) |>
    plotly::add_trace(
      data = data_ditch,
      y = ~real_height_cm,
      hoverinfo = "skip",
      type = "scatter",
      mode = "lines",
      line = list(width = 1.5, color = "#BB0000", dash = "dot"),
      name = "Average",
      visible = "legendonly"
    ) |>
    plotly::add_trace(
      data = data_cluster,
      y = ~real_height_cm,
      text = ~paste0("Date: ", date,
                     "<br>Height [cm]: ", real_height_cm,
                     "<br>Ideal Height [cm]: ", ideal_height_cm,
                     "<br>Ideal Irrigation: ", ideal_irrigation,
                     "<br>Ideal Draining: ", ideal_draining,
                     "<br>Real Irrigation: ", real_irrigation,
                     "<br>Real Draining: ", real_draining,
                     "<br>Plan Delay: ", plan_delay
      ),
      hoverinfo = "text",
      type = "scatter",
      mode = "lines",
      line = list(width = 2, color = "black"),
      name = "Simulated"
    ) |>
    plotly::layout(
      title = paste("Time Series of Height [cm]"),
      xaxis = list(title = "Date"),
      yaxis = list(title = "Height [cm]")
    )
}
