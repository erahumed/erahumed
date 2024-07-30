#' @export
plot.hb_global <- function(x, variable, ...) {
  var_lab <- hb_var_labels()[variable]
  is_imputed <- x[[paste0(variable, "_is_imputed")]]

  df_obs <- df_imp <- x[, ]
  df_obs[[variable]][is_imputed] <- NA
  df_imp[[variable]][!is_imputed] <- NA

  ## Possibly helpful
  # https://plotly-r.com/linking-views-with-shiny.html#shiny-plotly-inputs
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

plot_hb_local_cluster_levels <- function(data, cluster_id) {
  data <- data[data$cluster_id == cluster_id, ]
  data <- data[order(data$date), ]

  plotly::plot_ly(data = data, x = ~date) |>
    plotly::add_trace(
      y = ~real_height_cm,
      text = ~paste0("Date: ", date,
                     "<br>Height [cm]: ", real_height_cm,
                     "<br>Irrigation: ", irrigation,
                     "<br>Draining: ", draining),
      hoverinfo = "text",
      type = "scatter", mode = "lines",
      line = list(width = 2, color = "black"),
      name = "Simulated"
    ) |>
    plotly::add_trace(
      y = ~ideal_height_cm,
      type = "scatter", mode = "lines",
      line = list(width = 2, color = "#0000BB", dash = "dash"),
      name = "Ideal"
    ) |>
    plotly::layout(
      title = paste("Time Series of Height [cm]"),
      xaxis = list(title = "Date"),
      yaxis = list(title = "Height [cm]")
    )
}
