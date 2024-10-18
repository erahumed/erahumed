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

  switch(type,
         cluster_view = plot_erahumed_hbp_cluster_view(x, ...),
         map_view = plot_erahumed_hbp_map_view(x, ...)
         )
}

plot_erahumed_hbp_cluster_view <- function(x, ...)
{
  args <- list(...)

  data <- component_output(x)

  if ( !("cluster_id" %in% names(args)) )
    stop("Please specify cluster to plot through the 'cluster_id' argument.")

  cluster_id <- args$cluster_id

  data_cluster <- data[data$cluster_id == cluster_id, ]

  ditch <- data_cluster$ditch[1]
  tancat <- data_cluster$tancat[1]
  variety <- data_cluster$variety[1]

  data_ditch <- data[
    data$ditch == ditch & data$tancat == tancat & data$variety == variety,
    ] |>
    stats::aggregate(real_height_cm ~ date, data = _, FUN = mean)


  top_plot <- plotly::plot_ly(x = ~date) |>
    plotly::add_trace(
      data = data_cluster,
      y = ~ideal_height_cm,
      hoverinfo = "skip",
      type = "scatter",
      mode = "lines",
      line = list(width = 1.5, color = "black", dash = "dash"),
      name = "Water Level (ideal)"
    ) |>
    # plotly::add_trace(
    #   data = data_ditch,
    #   y = ~real_height_cm,
    #   hoverinfo = "skip",
    #   type = "scatter",
    #   mode = "lines",
    #   line = list(width = 1.5, color = "#BB0000", dash = "dot"),
    #   name = "Average",
    #   visible = "legendonly"
    # ) |>
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
      name = "Water Level (simulated)"
    )

  bottom_plot <- plotly::plot_ly(x = ~date) |>
    plotly::add_trace(
      data = data_cluster,
      y = ~real_inflow_cm,
      hoverinfo = "skip",
      #type = "scatter", mode = "lines", line = list(width = 1.5, color = "red"),
      type = "bar", marker = list(width = 3, color = "red"),
      name = "Inflow"
    ) |>
    plotly::add_trace(
      data = data_cluster,
      y = ~ I(-real_outflow_cm),
      hoverinfo = "skip",
      # type = "scatter", mode = "lines", line = list(width = 1.5, color = "red"),
      type = "bar", marker = list(width = 3, color = "red"),

      name = "Outflow"
    ) |>
    plotly::add_trace(
      data = data_cluster,
      y = ~petp_cm,
      # text = ~paste0("Date: ", date,
      #                "<br>Height [cm]: ", real_height_cm,
      #                "<br>Ideal Height [cm]: ", ideal_height_cm,
      #                "<br>Ideal Irrigation: ", ideal_irrigation,
      #                "<br>Ideal Draining: ", ideal_draining,
      #                "<br>Real Irrigation: ", real_irrigation,
      #                "<br>Real Draining: ", real_draining,
      #                "<br>Plan Delay: ", plan_delay
      # ),
      # hoverinfo = "text",
      hoverinfo = "skip",
      # type = "scatter", mode = "lines", line = list(width = 2, color = "blue"),
      type = "bar", marker = list(width = 1, color = "blue"),

      name = "P - ETP"
    )


  plotly::subplot(top_plot, bottom_plot, nrows = 2, shareX = TRUE) |>
    plotly::layout(
      title = paste("Time Series of Height [cm]"),
      xaxis = list(title = "Date"),
      yaxis = list(title = "Height [cm]")
    )
}

plot_erahumed_hbp_map_view <- function(x, ...) {
  stop("Not yet implemented.")
}
