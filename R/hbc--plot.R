plot_hbc <- function(simulation,
                     element_id = NULL,
                     type = c("storage", "flows"),
                     variable = c("depth", "volume"),
                     dygraph_group = NULL,
                     ...
                     )
{
  assert_erahumed_simulation(simulation)

  variable <- match.arg(variable)

  data <- get_output(simulation, "hbc")
  data$outflow_m3 <- -data$outflow_m3
  data$petp_m3 <- (data$petp_cm / 100) * data$area_m2

  if (is.null(element_id)) {
    element_id <- data$element_id[[1]]
    warning(paste0(
      "No cluster specified through the 'element_id' argument. ",
      "Plotting cluster '", element_id, "'."
    ))
  }


  data <- data[data$element_id == element_id, ]

  switch(match.arg(type),
         storage = plot_erahumed_hbc_storage(data,
                                             element_id = element_id,
                                             variable = variable,
                                             dygraph_group = dygraph_group),
         flows = plot_erahumed_hbc_flows(data,
                                         element_id = element_id,
                                         variable = variable,
                                         dygraph_group = dygraph_group)
  )
}

plot_erahumed_hbc_storage <- function(data, element_id, variable, dygraph_group)
{
  data$volume_m3 <- (data$height_eod_cm / 100) * data$area_m2
  data$depth_cm <- data$height_eod_cm

  y_var <- switch(variable, volume = "volume_m3", depth = "depth_cm")
  var_name <- switch(variable, depth = "Depth", volume = "Volume")
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

plot_erahumed_hbc_flows <- function(data, element_id, variable, dygraph_group)
{
  data$outflow_m3 <- -data$outflow_m3
  data$outflow_cm <- -data$outflow_cm
  data$petp_m3 <- (data$petp_cm / 100) * data$area_m2

  y_vars <- switch(variable,
                   volume = c("outflow_m3", "inflow_m3", "petp_m3"),
                   depth = c("outflow_cm", "inflow_cm", "petp_cm"))
  var_name <- switch(variable, depth = "Depth", volume = "Volume")
  var_units <- switch(variable, depth = "cm", volume = "m\u{00B3}")
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
