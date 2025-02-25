#' @noRd
plot.erahumed_hbc <- function(x,
                              element_id = NULL,
                              type = c("storage", "flows"),
                              variable = c("depth", "volume"),
                              dygraph_group = NULL,
                              ...
                              )
{
  variable <- match.arg(variable)

  data <- get_layer_output(x)
  data$outflow_m3 <- -data$outflow_m3_s * s_per_day()
  data$inflow_m3 <- data$inflow_m3_s * s_per_day()
  data$petp_m3 <- (data$petp_cm / 100) * data$area_m2

  if (is.null(element_id)) {
    element_id <- data$cluster_id[[1]]
    warning(paste0(
      "No cluster specified through the 'element_id' argument. ",
      "Plotting cluster '", element_id, "'."
    ))
  }


  data <- data[data$cluster_id == element_id, ]

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
  y_lab <- switch(variable, volume = "Volume [m\u{00B3}]", depth = "Depth [cm]")

  data[, c("date", y_var)] |>
    dygraphs::dygraph(group = dygraph_group) |>
    dygraphs::dyAxis("x", label = "Date") |>
    dygraphs::dyAxis("y", label = y_lab, axisLabelWidth = 80) |>
    dygraphs::dyLegend(show = "always") |>
    dygraphs::dyRangeSelector() |>
    dygraphs::dyUnzoom()

}

plot_erahumed_hbc_flows <- function(data, element_id, variable, dygraph_group)
{
  data$outflow_m3 <- -data$outflow_m3_s * s_per_day()
  data$outflow_cm <- -data$outflow_cm
  data$inflow_m3 <- data$inflow_m3_s * s_per_day()
  data$petp_m3 <- (data$petp_cm / 100) * data$area_m2

  y_vars <- switch(variable,
                   volume = c("outflow_m3", "inflow_m3"),
                   depth = c("outflow_cm", "inflow_cm"))
  y_lab <- switch(variable, depth = "Depth [cm]", volume = "Volume [m\u{00B3}]")

  ymin <- 1.25 * min(c(data[[ y_vars[1] ]], data[[ y_vars[3] ]]))
  ymax <- 1.25 * max(c(data[[ y_vars[2] ]], data[[ y_vars[3] ]]))

  data |>
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
