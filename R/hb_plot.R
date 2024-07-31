#' @export
plot.hb_global <- function(x, variable, ...) {
  var_lab <- hb_var_labels()[variable]
  is_imputed <- x[[paste0(variable, "_is_imputed")]]

  df_obs <- df_imp <- x[, ]
  df_obs[[variable]][is_imputed] <- NA
  df_imp[[variable]][!is_imputed] <- NA

  plotly::plot_ly() |>
    plotly::add_trace(
      data = df_obs, x = ~date, y = ~get(variable),
      type = "scatter", mode = "lines",
      line = list(color = "blue", width = 2, dash = "solid"),
      name = "Observed Data"
    ) |>
    plotly::add_trace(
      data = df_imp, x = ~date, y = ~get(variable),
      type = "scatter", mode = "lines",
      line = list(color = "red", width = 2, dash = "dash"),
      name = "Imputed Data"
    ) |>
    plotly::layout(
      title = paste("Time Series of", var_lab),
      xaxis = list(title = "Date"),
      yaxis = list(title = var_lab)
    )
}

#' @export
plot.hb_local <- function(x, type = c("cluster_levels", "map"), ...) {
  type <- match.arg(type)

  if (type == "map")
    stop("'map' plot not yet implemented.")

  args <- list(...)

  if (type == "cluster_levels") {
    if ( !("cluster_id" %in% names(args)) )
      stop("Please specify cluster to plot through the 'cluster_id' argument.")

    return(plot_hb_local_cluster_levels(data = x,
                                        cluster_id = args$cluster_id)
           )
  }
}

plot_hb_local_cluster_levels <- function(data, cluster_id)
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
                     "<br>Irrigation: ", irrigation,
                     "<br>Draining: ", draining,
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
