#' @noRd
plot.erahumed_hbc <- function(x,
                              element_id = NULL,
                              type = c("storage", "flows"),
                              dygraph_group = NULL,
                              ...
                              )
{
  data <- get_layer_output(x)
  data$volume_m3 <- (data$height_eod_cm / 100) * data$area_m2
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
                                             dygraph_group = dygraph_group),
         flows = plot_erahumed_hbc_flows(data,
                                         element_id = element_id,
                                         dygraph_group = dygraph_group)
  )
}

plot_erahumed_hbc_storage <- function(data, element_id, dygraph_group) {

  data[, c("date", "volume_m3")] |>
    dygraphs::dygraph(main = paste("Water storage of cluster", element_id),
                      group = dygraph_group
    ) |>
    dygraphs::dySeries(name = "volume_m3",
                       label = "Volume [m\u{00B3}]",
                       axis = "y") |>
    dygraphs::dyAxis("x", label = "Date") |>
    dygraphs::dyAxis("y", label = "Volume [m\u{00B3}]", axisLabelWidth = 80) |>
    dygraphs::dyLegend(show = "always") |>
    dygraphs::dyRangeSelector() |>
    dygraphs::dyUnzoom()
}

plot_erahumed_hbc_flows <- function(data, element_id, dygraph_group) {
  ymin <- 1.25 * min(c(data$outflow_m3, data$petp_m3))
  ymax <- 1.25 * max(c(data$inflow_m3, data$petp_m3))

  data |>
    (\(.) .[, c("date", "outflow_m3", "inflow_m3", "petp_m3")])() |>
    (\(.) xts::xts(., order.by = .$date))() |>
    dygraphs::dygraph(main = paste("Water flows of cluster", element_id),
                      group = dygraph_group) |>
    dygraphs::dyBarChart() |>
    dygraphs::dyAxis("x", label = "Date") |>
    dygraphs::dyAxis("y", label = "Volume [m\u{00B3}]", axisLabelWidth = 80,
                     valueRange = c(ymin, ymax)
    ) |>
    dygraphs::dyLegend(show = "always") |>
    dygraphs::dyRangeSelector() |>
    dygraphs::dyUnzoom()
}
