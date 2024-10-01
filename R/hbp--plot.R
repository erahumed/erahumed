#' Plot HBP model component output
#'
#' @description
#' Plot method for \link{hbp} model components.
#'
#' @param x An object of class `hbp`.
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
#' @return A plotly plot.
#'
#' @export
plot.erahumed_hbp <- function(x, type = c("cluster_view", "map_view"), ...) {
  type <- match.arg(type)

  if (type == "map_view")
    stop("'map_view' plot not yet implemented.")

  args <- list(...)

  data <- x$output

  if (type == "cluster_view") {
    if ( !("cluster_id" %in% names(args)) )
      stop("Please specify cluster to plot through the 'cluster_id' argument.")
    return( plot_hbp_cluster_view(data = data, cluster_id = args$cluster_id) )
  }
}

plot_hbp_cluster_view <- function(data, cluster_id)
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
