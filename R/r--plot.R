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

  ts_data <- xts::xts(r_output[-1], order.by = r_output$date)

  dygraphs::dygraph(ts_data) |>
    dygraphs::dyOptions(stackedGraph = TRUE, fillAlpha = 0.7) |>
    dygraphs::dyAxis("y", label = "-log(1-PAF)") |>
    dygraphs::dyRangeSelector() |>
    dygraphs::dyUnzoom()
}
