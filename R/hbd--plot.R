plot_hbd <- function(simulation,
                     element_id = NULL,
                     type = c("storage", "flows"),
                     variable = c("depth", "volume"),
                     dygraph_group = NULL,
                     ...)
{
  assert_erahumed_simulation(simulation)

  type <- match.arg(type)
  variable <- match.arg(variable)

  data <- get_output(simulation, "hbd")

  if (is.null(element_id)) {
    element_id <- data$element_id[[1]]
    warning(paste0(
      "No ditch specified through the 'element_id' argument. ",
      "Plotting ditch '", element_id, "'."
    ))
  }

  data <- data[data$element_id == element_id, ]

  switch(match.arg(type),
         storage = plot_erahumed_hbd_storage(data,
                                             element_id = element_id,
                                             variable = variable,
                                             dygraph_group = dygraph_group),
         flows = plot_erahumed_hbd_flows(data,
                                         element_id = element_id,
                                         variable = variable,
                                         dygraph_group = dygraph_group)
         )
}


plot_erahumed_hbd_storage <- function(data, element_id, variable, dygraph_group)
{
  data$depth_cm <- data$level_m * 1e2

  y_var <- switch(variable, volume = "volume_m3", depth = "depth_cm")
  var_name <- switch(variable, depth = "Depth", volume = "Volume")
  var_units <- switch(variable, depth = "cm", volume = "m\u{00B3}")
  y_lab <- paste0(var_name, " [", var_units, "]")
  value_fmt <- "function(d) { return d.toPrecision(3) + ' %s'; }" |>
    sprintf(var_units) |>
    htmlwidgets::JS()

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

plot_erahumed_hbd_flows <- function(data, element_id, variable, dygraph_group)
{
  data$outflow_m3 <- -data$outflow_lake_m3
  data$outflow_cm <- 1e2 * data$outflow_m3 / data$area_m2
  data$inflow_m3 <- -data$outflow_m3
  data$inflow_cm <- -data$outflow_cm

  y_vars <- switch(variable,
                   volume = c("outflow_m3", "inflow_m3"),
                   depth = c("outflow_cm", "inflow_cm"))
  var_name <- switch(variable, depth = "Depth", volume = "Volume")
  var_units <- switch(variable, depth = "cm", volume = "m\u{00B3}")
  y_lab <- paste0(var_name, " [", var_units, "]")
  value_fmt <- "function(d) { return d.toPrecision(3) + ' %s'; }" |>
    sprintf(var_units) |>
    htmlwidgets::JS()

  ymin <- 1.25 * min(data[[ y_vars[1] ]])
  ymax <- 1.25 * max(data[[ y_vars[2] ]])

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
    dygraphs::dySeries(y_vars[[2]], label = "Inflow", color = "#000099")
}

