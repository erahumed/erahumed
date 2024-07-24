# TODO: should we export these S3 methods?

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
