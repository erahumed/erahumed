#' @noRd
#' @noRd
plot.erahumed_hbd <- function(x,
                              element_id = NULL,
                              type = c("storage", "flows"),
                              dygraph_group = NULL,
                              ...)
{
  data <- get_layer_output(x)

  if (is.null(element_id)) {
    element_id <- data$ditch[[1]]
    warning(paste0(
      "No ditch specified through the 'element_id' argument. ",
      "Plotting ditch '", element_id, "'."
    ))
  }

  data <- data[data$ditch == element_id, ]

  switch(match.arg(type),
         storage = plot_erahumed_hbd_storage(data,
                                             element_id = element_id,
                                             dygraph_group = dygraph_group),
         flows = plot_erahumed_hbd_flows(data,
                                         element_id = element_id,
                                         dygraph_group = dygraph_group)
         )
}


plot_erahumed_hbd_storage <- function(data, element_id, dygraph_group) {
  data[, c("date", "volume_m3")] |>
    dygraphs::dygraph(main = paste("Water storage of ditch", element_id),
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

plot_erahumed_hbd_flows <- function(data, element_id, dygraph_group) {
  ymin <- -1.25 * max(data$outflow_lake_m3)
  ymax <- 1.25 * max(data$outflow_lake_m3)

  data$outflow_lake_m3 <- -data$outflow_lake_m3

  data |>
    (\(.) .[, c("date", "outflow_lake_m3", "inflow_clusters_m3", "inflow_external_m3")])() |>
    (\(.) xts::xts(., order.by = .$date))() |>
    dygraphs::dygraph(main = paste("Water flows of ditch", element_id),
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

