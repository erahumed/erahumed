plot_risk <- function(r_output, type = c("chronic", "acute")) {
  type <- match.arg(type)

  var <- switch(type,
                chronic = "paf_transf_chronic",
                acute = "paf_transf_acute"
                )

  paf_chronic <- paf_acute <- paf_transf_chronic <- paf_transf_acute <- NULL

  r_output <- r_output |>
    data.table::as.data.table() |>
    (function(dt) {
      dt[,
       let(paf_transf_chronic = neg_log_surv(paf_chronic),
           paf_transf_acute = neg_log_surv(paf_acute)
           )
      ][,
        list(paf_transf_chronic = mean(paf_transf_chronic),
             paf_transf_acute = mean(paf_transf_acute)
             )
        , by = c("date", "tmoa")
      ]
    })() |>
    as.data.frame() |>
    (\(.) .[, c("date", "tmoa", var)])() |>
    stats::reshape(timevar = "tmoa", idvar = "date", direction = "wide")

  colnames(r_output) <- gsub("^paf_transf_chronic\\.", "", colnames(r_output))

  r_output$msPAF <- rowSums(r_output[, -1, drop = FALSE], na.rm = TRUE)

  ts_data <- xts::xts(r_output[-1], order.by = r_output$date)

  # Get column names
  cols <- names(ts_data)
  cols_without_total <- setdiff(cols, "msPAF")

  ts_data <- xts::xts(r_output[-1], order.by = r_output$date)

  g <- dygraphs::dygraph(ts_data) |>
    dygraphs::dySeries(axis = "y2") |>
    dygraphs::dyOptions(stackedGraph = TRUE, fillAlpha = 0.7) |>
    dygraphs::dyAxis("y2",
                     label = "Toxicity Contribution [-log(1-PAF\u{1D62})]",
                     independentTicks = FALSE) |>
    dygraphs::dyAxis("y",
                     label = "msPAF",
                     axisLabelFormatter = "
                       function(value, granularity, opts, dygraph) {
                         return (100*(1 - Math.exp(-value))).toFixed(1) + '%';
                       }
                     ",
                     valueFormatter = "
                       function(value) {
                         return (100*(1 - Math.exp(-value))).toFixed(1) + '% species';
                       }
                     "
                     ) |>
    dygraphs::dyRangeSelector() |>
    dygraphs::dyUnzoom()

  for (col in cols_without_total) {
    g <- g |> dygraphs::dySeries(col, axis = "y2")
  }

  # Map "Total" to y (transformed)
  g <- g |> dygraphs::dySeries("msPAF", axis = "y", color = "black", fillGraph = FALSE)

  g
}
