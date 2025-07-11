ct_plot_time_series_density <- function(data,
                                        element_id = NULL,
                                        chemical_ids = NULL,
                                        compartment = c("water", "sediment"),
                                        dygraph_group,
                                        chemical_db)
{
  if (length(chemical_db) == 0) return(NULL)

  compartment <- match.arg(compartment)

  if (!is.null(chemical_ids)) {
    assert_integer_vector(chemical_ids)
    data <- data[data$chemical_id %in% chemical_ids, ]
  }

  if (compartment == "water") {
    data$density <- data$cw_kg_m3 * 1e6
    data$density[is.na(data$density)] <- 0
    units <- "\u{03BC}g / L"
  } else {
    data$density <- data$cs_g_kg * 1e3
    units <- "\u{03BC}g / g"
  }

  # Keep only needed columns
  data <- data[, c("date", "chemical_id", "density")]

  # Determine ordered chemical_ids by name
  chem_ids <- sort(unique(data$chemical_id))
  chem_names <- sapply(chem_ids, \(id) chemical_db[[id]][["display_name"]])
  chem_ids_ordered <- chem_ids[order(chem_names)]
  chem_names_ordered <- chem_names[order(chem_names)]

  # Pivot
  wide_dt <- data |>
    data.table::as.data.table() |>
    data.table::dcast(date ~ chemical_id, value.var = "density") |>
    data.table::setcolorder(c("date", as.character(chem_ids_ordered)))


  ts_plot <- xts::xts(wide_dt[, -1, with = FALSE], order.by = wide_dt$date)

  value_fmt <- sprintf("function(d) { return d.toPrecision(3) + ' %s'; }", units)

  g <- dygraphs::dygraph(ts_plot, group = dygraph_group) |>
    dygraphs::dyAxis("x", label = "Date") |>
    dygraphs::dyAxis("y",
                     label = paste0("Concentration [", units, "]"),
                     axisLabelWidth = 80,
                     valueFormatter = value_fmt
    ) |>
    dygraphs::dyLegend(show = "always",
                       showZeroValues = TRUE,
                       labelsSeparateLines = TRUE) |>
    dygraphs::dyRangeSelector() |>
    dygraphs::dyUnzoom() |>
    dygraphs::dyOptions(strokeWidth = 1.5)

  for (id in chem_ids_ordered) {
    series_id <- as.character(id)
    color <- chemical_color_map(chemical_db)[[id]]
    if (is.null(color) || is.na(color)) color <- "#888888"
    g <- g |>
      dygraphs::dySeries(series_id, label = chem_names[[id]], color = color)
  }

  g
}
