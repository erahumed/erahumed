ct_plot_time_series_density <- function(data,
                                        element_id = NULL,
                                        chemical_ids = NULL,
                                        compartment = c("water", "sediment"),
                                        dygraph_group,
                                        chemical_db)
{
  compartment <- match.arg(compartment)

  # Vectorized name lookup (assumes get_name is vectorized or vectorizable)
  #data$chemical_name <- get_name(data$chemical_id)

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

  # Pivot by chemical_id to guarantee uniqueness
  data <- data[, c("date", "chemical_id", "density")]

  plot_df <- stats::reshape(data,
                            idvar = "date",
                            timevar = "chemical_id",
                            direction = "wide",
                            sep = "") |>
    (\(df) {
      # Rename columns from e.g., "density5" to "chemical_name (5)"
      names(df) <- gsub("density", "", names(df), fixed = TRUE)
      col_ids <- setdiff(names(df), "date")
      col_labels <- sapply(col_ids, \(id) chemical_db[[id]][["display_name"]])
      if (anyDuplicated(col_labels)) {
        col_labels <- paste0(col_labels, " (", col_ids, ")")
      }
      names(df)[match(col_ids, names(df))] <- col_labels
      df[, c("date", sort(col_labels))]
    })()

  chemical_names <- setdiff(names(plot_df), "date")

  # Color mapping should still use readable labels
  # dy_colors <- chemical_color_map()[get_name(as.integer(gsub(".*\\((\\d+)\\)", "\\1", chemical_names)))] |> unname()

  value_fmt <- sprintf("function(d) { return d.toPrecision(3) + ' %s'; }", units)

  dygraphs::dygraph(plot_df, group = dygraph_group) |>
    dygraphs::dyAxis("x", label = "Date") |>
    dygraphs::dyAxis("y",
                     label = paste0("Concentration [", units, "]"),
                     axisLabelWidth = 80,
                     valueFormatter = value_fmt
    ) |>
    dygraphs::dyLegend(show = "always",
                       showZeroValues = FALSE,
                       labelsSeparateLines = TRUE) |>
    dygraphs::dyRangeSelector() |>
    dygraphs::dyUnzoom() |>
    #dygraphs::dyOptions(colors = dy_colors) |>  TODO:
    identity()
}
