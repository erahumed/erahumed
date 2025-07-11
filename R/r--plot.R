plot_risk_stacked <- function(r_output,
                      type = c("chronic", "acute"),
                      dygraph_group = NULL,
                      chemical_db)
{
  if (length(chemical_db) == 0) return(NULL)

  type <- match.arg(type)

  var <- switch(type,
                chronic = "mspaf_chronic",
                acute = "mspaf_acute")

  paf_chronic <- paf_acute <- mspaf_acute <- mspaf_chronic <- stressor_type <- NULL

  r_output <- data.table::as.data.table(r_output)

  mspaf_df <- r_output[
    stressor_type == "tmoa"
  ][,
    list(mspaf_acute = 1 - prod(1 - paf_acute),
         mspaf_chronic = 1 - prod(1 - paf_chronic)
         ),
    by = "date"
  ]

  r_output <- merge(r_output, mspaf_df, by = "date")

  df_plot <- r_output[
    stressor_type == "chemical"
  ][,
    let(
        mspaf_chronic = paf_chronic / sum(paf_chronic) * mspaf_chronic,
        mspaf_acute = paf_acute / sum(paf_acute) * mspaf_acute
        ),
    by = "date"
  ] |>
    as.data.frame() |>
    (\(.) .[, c("date", "stressor", var)])() |>
    stats::reshape(timevar = "stressor", idvar = "date", direction = "wide") |>
    (\(.) .[, order(colnames(.))])()

  colnames(df_plot) <- gsub("^mspaf_chronic\\.", "", colnames(df_plot))
  colnames(df_plot) <- gsub("^mspaf_acute\\.", "", colnames(df_plot))

  # Compute total msPAF (sum of stacked areas)
  df_plot$Total <- rowSums(df_plot[, -1, drop = FALSE], na.rm = TRUE)

  ts_data <- xts::xts(df_plot[-1], order.by = df_plot$date)

  # Extract column names without "msPAF"
  cols_without_total <- setdiff(colnames(ts_data), "Total")

  value_fmt <- "function(d) {return (100*d).toFixed(2) + ' %';}"
  axis_fmt <- value_fmt

  max_y <- max(ts_data$Total, na.rm = TRUE) * 1.01  # Slightly larger

  g <- dygraphs::dygraph(ts_data, group = dygraph_group) |>
    dygraphs::dyOptions(stackedGraph = TRUE, fillAlpha = 0.7) |>
    dygraphs::dyAxis("x", label = "Date") |>
    dygraphs::dyAxis("y",
                     label = "Potentially Affected Fraction of Species [%]",
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
  for (col in seq_along(cols_without_total)) {
    color <- chemical_color_map(chemical_db)[col]
    if (is.na(color)) color <- "#888888"
    g <- g |> dygraphs::dySeries(cols_without_total[col], axis = "y", color = color)
  }

  # Add the msPAF line separately on a second y-axis
  g <- g |> dygraphs::dySeries("Total", axis = "y2", color = "black", strokeWidth = 2, fillGraph = FALSE)

  g
}



plot_risk <- function(r_output,
                      type = c("chronic", "acute"),
                      dygraph_group = NULL,
                      chemical_db
)
{
  if (length(chemical_db) == 0) return(NULL)

  type <- match.arg(type)
  var <- paste0("paf_", type)

  stressor_type <- stressor_id <- NULL

  r_output_dt <- data.table::as.data.table(r_output)

  ## --- Create lookup from stressor_id to stressor_name ---
  chem_ids <- unique(r_output_dt[stressor_type == "chemical", stressor_id]) |>
    as.numeric()
  id_to_name <- vapply(chem_ids, function(id) {
    chemical_db[[id]][["display_name"]]
  }, FUN.VALUE = character(1))
  names(id_to_name) <- chem_ids

  ## --- Wide table for chemical stressors ---
  chem_wide <- r_output_dt[stressor_type == "chemical"] |>
    data.table::dcast(date ~ stressor_id, value.var = var)

  ## --- Compute msPAF from TMoA stressors ---
  tmoa_dt <- r_output_dt[stressor_type == "tmoa"]
  msPAF_dt <- tmoa_dt[,
                      list(msPAF = 1 - prod(1 - get(var))),
                      by = "date"
                      ]

  ## --- Merge into single data.frame ---
  df_plot <- merge(chem_wide, msPAF_dt, by = "date", all = TRUE)

  ## --- Convert to xts ---
  ts_plot <- xts::xts(df_plot[, -1, with = FALSE], order.by = df_plot$date)

  ## --- Plot formatting ---
  value_fmt <- "function(d) {return (100*d).toFixed(2) + ' %';}"
  axis_fmt <- value_fmt
  ymax <- max(ts_plot, na.rm = TRUE) * 1.05

  g <- dygraphs::dygraph(ts_plot, group = dygraph_group) |>
    dygraphs::dyOptions(strokeWidth = 1.5) |>
    dygraphs::dyAxis("x", label = "Date") |>
    dygraphs::dyAxis("y",
                     label = "Potentially Affected Fraction of Species [%]",
                     axisLabelWidth = 80,
                     valueRange = c(0, ymax),
                     axisLabelFormatter = axis_fmt,
                     valueFormatter = value_fmt) |>
    dygraphs::dyLegend(show = "always",
                       showZeroValues = FALSE,
                       labelsSeparateLines = TRUE) |>
    dygraphs::dyRangeSelector()

  for (id in names(id_to_name)) {
    if (id %in% colnames(ts_plot)) {
      color <- chemical_color_map(chemical_db)[[as.numeric(id)]]
      if (is.null(color) || is.na(color)) color <- "#888888"
      g <- g |>
        dygraphs::dySeries(id, label = id_to_name[[id]], color = color)
    }
  }


  ## --- Highlight msPAF ---
  if ("msPAF" %in% colnames(ts_plot)) {
    g <- g |> dygraphs::dySeries("msPAF",
                                 label = "msPAF",
                                 color = "black",
                                 strokeWidth = 3)
  }

  g
}


