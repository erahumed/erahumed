plot_risk <- function(r_output,
                      type = c("chronic", "acute"),
                      dygraph_group = NULL)
{
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

  max_y1 <- max(ts_data$msPAF, na.rm = TRUE)
  max_y2 <- neg_log_surv_inv(max_y1)

  g <- dygraphs::dygraph(ts_data, group = dygraph_group) |>
    dygraphs::dyOptions(stackedGraph = TRUE, fillAlpha = 0.7) |>
    dygraphs::dyAxis("y",
                     label = "Toxicity Contribution [-log(1-PAF\u{1D62})]",
                     axisLabelWidth = 80,
                     valueRange =  c(0, max_y1)
                     ) |>
    dygraphs::dyAxis("y2",
                     label = "Total risk [msPAF]",
                     axisLabelFormatter = "
                       function(value, granularity, opts, dygraph) {
                         return (100*(1 - Math.exp(-value))).toFixed(1) + '%';
                       }
                     ",
                     valueFormatter = "
                       function(value) {
                         return (100*(1 - Math.exp(-value))).toFixed(1) + '% species';
                       }
                     ",
                     independentTicks = FALSE,
                     valueRange = c(0, max_y1)
                     ) |>
    dygraphs::dyLegend(showZeroValues = FALSE, labelsSeparateLines = TRUE) |>
    dygraphs::dyRangeSelector() |>
    dygraphs::dyUnzoom()

  for (col in cols_without_total) {
    g <- g |> dygraphs::dySeries(col, axis = "y")
  }

  g <- g |>
    dygraphs::dySeries("msPAF", axis = "y2", color = "black", fillGraph = FALSE)

  g
}
