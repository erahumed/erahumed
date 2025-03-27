plot_risk <- function(r_output,
                      type = c("chronic", "acute"),
                      dygraph_group = NULL)
{
  type <- match.arg(type)

  var <- switch(type,
                chronic = "mspaf_chronic",
                acute = "mspaf_acute"
  )

  paf_chronic <- paf_acute <- NULL

  r_output <- r_output |>
    data.table::as.data.table() |>
    (function(dt) {
      dt[, let(
        mspaf_chronic = paf_chronic / sum(paf_chronic) * (1 - prod(1 - paf_chronic)),
        mspaf_acute = paf_acute / sum(paf_acute) * (1 - prod(1 - paf_acute))
      ), by = "date"]
    })() |>
    as.data.frame() |>
    (\(.) .[, c("date", "tmoa", var)])() |>
    stats::reshape(timevar = "tmoa", idvar = "date", direction = "wide")

  colnames(r_output) <- gsub("^mspaf_chronic\\.", "", colnames(r_output))
  colnames(r_output) <- gsub("^mspaf_acute\\.", "", colnames(r_output))

  # Compute total msPAF (sum of stacked areas)
  r_output$Total <- rowSums(r_output[, -1, drop = FALSE], na.rm = TRUE)

  ts_data <- xts::xts(r_output[-1], order.by = r_output$date)

  # Extract column names without "msPAF"
  cols_without_total <- setdiff(colnames(ts_data), "Total")

  value_fmt <- "function(d) {return (100*d).toFixed(2) + ' %';}" |>
    htmlwidgets::JS()
  axis_fmt <- value_fmt

  max_y <- max(ts_data$Total, na.rm = TRUE)

  g <- dygraphs::dygraph(ts_data, group = dygraph_group) |>
    dygraphs::dyOptions(stackedGraph = TRUE, fillAlpha = 0.7) |>
    dygraphs::dyAxis("x", label = "Date") |>
    dygraphs::dyAxis("y",
                     label = "msPAF [% of species]",
                     axisLabelWidth = 80,
                     axisLabelFormatter = axis_fmt,
                     valueFormatter = value_fmt,
                     valueRange =  c(0, max_y)
                     ) |>
    dygraphs::dyAxis("y2",
                     valueFormatter = value_fmt,
                     independentTicks = FALSE,
                     valueRange = c(0, max_y)
                     ) |>
    dygraphs::dyLegend(show = "always",
                       showZeroValues = FALSE,
                       labelsSeparateLines = TRUE
                       ) |>
    dygraphs::dyRangeSelector() |>
    dygraphs::dyUnzoom() |>
    dygraphs::dyCSS(textConnection(
      ".dygraph-axis-label-y2 { display: none; }"
      )) # Hide ticks and label for y2

  # Add each individual series as part of the stacked graph
  for (col in cols_without_total) {
    color <- chemical_color_map()[col]
    if (is.na(color)) color <- "#888888"
    g <- g |> dygraphs::dySeries(col, axis = "y", color = color)
  }

  # Add the msPAF line separately on a second y-axis
  g <- g |> dygraphs::dySeries("Total", axis = "y2", color = "black", strokeWidth = 2, fillGraph = FALSE)

  g
}
