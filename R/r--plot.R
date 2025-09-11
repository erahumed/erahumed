plot_risk <- function(r_output,
                      type = c("chronic", "acute"),
                      method = c("paf", "rq"),
                      dygraph_group = NULL,
                      chemical_db)
{
  if (length(chemical_db) == 0) return(NULL)

  type <- match.arg(type)
  method <- match.arg(method)

  var <- paste0(tolower(method), "_", type)  # gives "paf_chronic", "rq_acute", etc.

  stressor_type <- stressor_id <- NULL

  r_output_dt <- data.table::as.data.table(r_output)

  ## --- Create lookup from stressor_id to stressor_name ---
  chem_ids <- unique(r_output_dt[stressor_type == "chemical", stressor_id]) |>
    as.numeric()
  chem_names <- sapply(chem_ids, \(id) chemical_db[[id]][["display_name"]])
  chem_ids_ordered <- chem_ids[order(chem_names)]

  ## --- Wide table for chemical stressors ---
  chem_wide <- r_output_dt[stressor_type == "chemical"] |>
    data.table::dcast(date ~ stressor_id, value.var = var) |>
    data.table::setcolorder(c("date", as.character(chem_ids_ordered)))

  ## --- Compute combined metric (e.g. msPAF) if using "paf" ---
  if (method == "paf") {
    tmoa_dt <- r_output_dt[stressor_type == "tmoa"]
    msPAF_dt <- tmoa_dt[,
                        list(msPAF = 1 - prod(1 - get(var))),
                        by = "date"
    ]
    df_plot <- merge(msPAF_dt, chem_wide, by = "date", all = TRUE)
  } else {
    df_plot <- chem_wide
  }

  ## --- Convert to xts ---
  ts_plot <- xts::xts(df_plot[, -1, with = FALSE], order.by = df_plot$date)

  ## --- Plot formatting ---
  if (method == "paf") {
    value_fmt <- "function(d) {return (100*d).toFixed(2) + ' %';}"
    axis_label <- "Potentially Affected Fraction of Species [%]"
    value_range <- c(0, max(ts_plot, na.rm = TRUE) * 1.05)
  } else {
    value_fmt <- "function(d) {return d.toFixed(2);}"
    axis_label <- "Risk Quotient (PEC / PNEC) [1]"
    value_range <- c(0, max(ts_plot, na.rm = TRUE) * 1.05)
  }

  g <- dygraphs::dygraph(ts_plot, group = dygraph_group) |>
    dygraphs::dyOptions(strokeWidth = 1.5) |>
    dygraphs::dyAxis("x", label = "Date") |>
    dygraphs::dyAxis("y",
                     label = axis_label,
                     axisLabelWidth = 80,
                     valueRange = value_range,
                     axisLabelFormatter = value_fmt,
                     valueFormatter = value_fmt) |>
    dygraphs::dyLegend(show = "always",
                       showZeroValues = TRUE,
                       labelsSeparateLines = TRUE) |>
    dygraphs::dyRangeSelector()

  for (id in chem_ids_ordered) {
    series_id <- as.character(id)
    color <- chemical_color_map(chemical_db)[[id]]
    if (is.null(color) || is.na(color)) color <- "#888888"
    g <- g |>
      dygraphs::dySeries(series_id, label = chem_names[[id]], color = color)
  }

  ## --- Highlight msPAF ---
  if (method == "paf" && "msPAF" %in% colnames(ts_plot)) {
    g <- g |> dygraphs::dySeries("msPAF",
                                 label = "Combined effect",
                                 color = "black",
                                 strokeWidth = 3)
  }

  g
}
