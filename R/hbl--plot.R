#' @noRd
plot.erahumed_hbl <- function(x,
                              type = c("storage", "flows"),
                              dygraph_group = NULL,
                              ...)
{
  type <- match.arg(type)

  switch(type,
         storage = plot_erahumed_hbl_storage(x, dygraph_group = dygraph_group),
         flows = plot_erahumed_hbl_flows(x, dygraph_group = dygraph_group)
         )
}

plot_erahumed_hbl_storage <- function(x, dygraph_group) {
  df <- get_layer_output(x)

  main <- "Time Series of water storage"

  df[, c("date", "volume")] |>
    dygraphs::dygraph(main = main, group = dygraph_group) |>
    dygraphs::dyAxis("x", label = "Date") |>
    dygraphs::dyAxis("y", label = "Volume [m\u{00B3}]", axisLabelWidth = 80) |>
    dygraphs::dyLegend(show = "always") |>
    dygraphs::dyRangeSelector() |>
    dygraphs::dyUnzoom()
}

plot_erahumed_hbl_flows <- function(x, dygraph_group) {
  df <- get_layer_output(x)

  df$outflow_m3 <- -df$outflow_total * s_per_day()
  df$inflow_m3 <- df$inflow_total * s_per_day()
  df$petp_m3 <- df$volume_change_petp

  ymin <- 1.25 * min(c(df$outflow_m3, df$petp_m3))
  ymax <- 1.25 * max(c(df$inflow_m3, df$petp_m3))

  main <- "Time Series of water flows"

  df |>
    (\(.) .[, c("date", "outflow_m3", "inflow_m3", "petp_m3")])() |>
    (\(.) xts::xts(., order.by = .$date))() |>
    dygraphs::dygraph(main = main, group = dygraph_group) |>
    dygraphs::dyBarChart() |>
    dygraphs::dyAxis("x", label = "Date") |>
    dygraphs::dyAxis("y", label = "Volume [m\u{00B3}]", axisLabelWidth = 80,
                     valueRange = c(ymin, ymax)
                     ) |>
    dygraphs::dyLegend(show = "always") |>
    dygraphs::dyRangeSelector() |>
    dygraphs::dyUnzoom()
}
