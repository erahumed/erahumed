plot_hbl <- function(simulation,
                     type = c("storage", "flows"),
                     variable = c("depth", "volume"),
                     dygraph_group = NULL,
                     ...)
{
  assert_erahumed_simulation(simulation)
  type <- match.arg(type)
  variable <- match.arg(variable)
  data <- get_raw_output(simulation, "hbl")
  sc_slope <- get_input(simulation, "storage_curve_slope_m2")

  switch(type,
         storage = plot_erahumed_hbl_storage(data, variable, dygraph_group = dygraph_group),
         flows = plot_erahumed_hbl_flows(data, variable, sc_slope, dygraph_group = dygraph_group)
         )
}

plot_erahumed_hbl_storage <- function(data, variable, dygraph_group) {

  data$depth_cm <- data$depth_m * 100

  y_var <- switch(variable, depth = "depth_cm", volume = "volume_m3")
  var_name <- switch(variable, depth = "Water depth", volume = "Water volume")
  var_units <- switch(variable, depth = "cm", volume = "m\u{00B3}")
  y_lab <- paste0(var_name, " [", var_units, "]")
  value_fmt <- "function(d) { return d.toPrecision(3) + ' %s'; }" |>
    sprintf(var_units)

  data[, c("date", y_var)] |>
    dygraphs::dygraph(group = dygraph_group) |>
    dygraphs::dyAxis("x", label = "Date") |>
    dygraphs::dyAxis("y",
                     label = y_lab,
                     axisLabelWidth = 80,
                     valueFormatter = value_fmt
                     ) |>
    dygraphs::dyLegend(show = "always", labelsSeparateLines = TRUE) |>
    dygraphs::dyRangeSelector() |>
    dygraphs::dyUnzoom() |>
    dygraphs::dySeries(y_var, label = var_name, color = "black")
}


plot_erahumed_hbl_flows <- function(data, variable, sc_slope, dygraph_group)
{
  data$petp_m3 <- data$volume_change_petp_m3
  data$petp_cm <- 0.1 * (data$precipitation_mm - data$evapotranspiration_mm)

  data$outflow_m3 <- -data$outflow_total_m3 * s_per_day()
  data$inflow_m3 <- data$inflow_total_m3 * s_per_day()
  data$outflow_cm <- 100 * data$outflow_m3 / sc_slope
  data$inflow_cm <- 100 * data$inflow_m3 / sc_slope

  y_vars <- switch(variable,
                   volume = c("outflow_m3", "inflow_m3", "petp_m3"),
                   depth = c("outflow_cm", "inflow_cm", "petp_cm"))
  var_name <- switch(variable, depth = "Flow rate (depth)", volume = "Flow rate (volume)")
  var_units <- switch(variable, depth = "cm / day", volume = "m\u{00B3} / day")
  y_lab <- paste0(var_name, " [", var_units, "]")
  value_fmt <- "function(d) { return d.toPrecision(3) + ' %s'; }" |>
    sprintf(var_units)

  ymin <- 1.25 * min(c(data[[ y_vars[1] ]], data[[ y_vars[3] ]]))
  ymax <- 1.25 * max(c(data[[ y_vars[2] ]], data[[ y_vars[3] ]]))

  data |>
    (\(.) .[, c("date", y_vars)])() |>
    dygraphs::dygraph(group = dygraph_group) |>
    dygraphs::dyBarChart() |>
    dygraphs::dyAxis("x", label = "Date") |>
    dygraphs::dyAxis("y",
                     label = y_lab,
                     axisLabelWidth = 80,
                     valueFormatter = value_fmt,
                     valueRange = c(ymin, ymax)
                     ) |>
    dygraphs::dyLegend(show = "always", labelsSeparateLines = TRUE) |>
    dygraphs::dyRangeSelector() |>
    dygraphs::dyUnzoom() |>
    dygraphs::dySeries(y_vars[[1]], label = "Outflow", color = "#000099") |>
    dygraphs::dySeries(y_vars[[2]], label = "Inflow", color = "#000099") |>
    dygraphs::dySeries(y_vars[[3]], label = "PET", color = "#DD0000")
}
