ct_plot_time_series_density <- function(data,
                                        element_id = NULL,
                                        compartment = c("water", "sediment"),
                                        chemicals,
                                        dygraph_group)
{
  compartment <- match.arg(compartment)

  if (!is.null(chemicals)) {
    assert_character(chemicals)
    data <- data |> (\(.) .[.$chemical %in% chemicals, ])()
  }

  chemicals <- unique(data$chemical)

  if (compartment == "water") {
    data$density <- data$cw_kg_m3 * 1e6
    data <- data[, c("date", "chemical", "density")]
    units <- "\u{03BC}g / L"

  } else {
    data$density <- data$cs_g_kg * 1e3
    data <- data[, c("date", "chemical", "density")]
    units <- "\u{03BC}g / g"
  }

  value_fmt <- "function(d) { return d.toPrecision(3) + ' %s'; }" |>
    sprintf(units) |>
    htmlwidgets::JS()


  plot_df <- data |>
    stats::reshape(idvar = "date",
                   timevar = "chemical",
                   direction = "wide",
                   sep = ""
    ) |>
    (function(df) {
      names(df) <- gsub("density", "", names(df), fixed = TRUE)
      return(df)
      })()

  chemical_names <- setdiff(names(plot_df), "date")
  dy_colors <- chemical_color_map()[chemical_names] |> unname()

  dygraphs::dygraph(plot_df, group = dygraph_group) |>
    dygraphs::dyAxis("x", label = "Date") |>
    dygraphs::dyAxis("y",
                     label = paste0("Density [", units, "]"),
                     axisLabelWidth = 80,
                     valueFormatter = value_fmt
                     ) |>
    dygraphs::dyLegend(show = "always",
                       showZeroValues = FALSE,
                       labelsSeparateLines = TRUE) |>
    dygraphs::dyRangeSelector() |>
    dygraphs::dyUnzoom() |>
    dygraphs::dyOptions(colors = dy_colors)

}
