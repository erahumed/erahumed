#' @noRd
plot.erahumed_hbl <- function(x,
                              type = c("storage", "flows"),
                              variable = c("depth", "volume"),
                              dygraph_group = NULL,
                              ...)
{
  type <- match.arg(type)
  variable <- match.arg(variable)

  switch(type,
         storage = plot_erahumed_hbl_storage(x, variable, dygraph_group = dygraph_group),
         flows = plot_erahumed_hbl_flows(x, variable, dygraph_group = dygraph_group)
         )
}

plot_erahumed_hbl_storage <- function(x, variable, dygraph_group) {
  df <- get_layer_output(x)

  df$volume_m3 <- df$volume
  df$level_cm <- df$level * 100

  df_var <- switch(variable, depth = "level_cm", volume = "volume_m3")
  y_lab <- switch(variable, depth = "Depth [cm]", volume = "Volume [m\u{00B3}]")

  df[, c("date", df_var)] |>
    dygraphs::dygraph(group = dygraph_group) |>
    dygraphs::dyAxis("x", label = "Date") |>
    dygraphs::dyAxis("y", label = y_lab, axisLabelWidth = 80) |>
    dygraphs::dyLegend(show = "always") |>
    dygraphs::dyRangeSelector() |>
    dygraphs::dyUnzoom()
}

plot_erahumed_hbl_flows <- function(x, variable, dygraph_group) {
  df <- get_layer_output(x)

  df$petp_m3 <- df$volume_change_petp
  df$petp_cm <- 0.1 * (df$precipitation_mm - df$evapotranspiration_mm)

  sc_slope <- df$outflow_cm <- get_layer_parameters(x)[["storage_curve_slope_m2"]]
  df$outflow_m3 <- -df$outflow_total * s_per_day()
  df$inflow_m3 <- df$inflow_total * s_per_day()
  df$outflow_cm <- 100 * df$outflow_m3 / sc_slope
  df$inflow_cm <- 100 * df$inflow_m3 / sc_slope

  y_vars <- switch(variable,
                   volume = c("outflow_m3", "inflow_m3", "petp_m3"),
                   depth = c("outflow_cm", "inflow_cm", "petp_cm"))
  y_lab <- switch(variable, depth = "Depth [cm]", volume = "Volume [m\u{00B3}]")

  ymin <- 1.25 * min(c(df[[ y_vars[1] ]], df[[ y_vars[3] ]]))
  ymax <- 1.25 * max(c(df[[ y_vars[2] ]], df[[ y_vars[3] ]]))

  df |>
    (\(.) .[, c("date", y_vars)])() |>
    (\(.) xts::xts(., order.by = .$date))() |>
    dygraphs::dygraph(group = dygraph_group) |>
    dygraphs::dyBarChart() |>
    dygraphs::dyAxis("x", label = "Date") |>
    dygraphs::dyAxis("y", label = y_lab, axisLabelWidth = 80,
                     valueRange = c(ymin, ymax)
                     ) |>
    dygraphs::dyLegend(show = "always") |>
    dygraphs::dyRangeSelector() |>
    dygraphs::dyUnzoom()
}
