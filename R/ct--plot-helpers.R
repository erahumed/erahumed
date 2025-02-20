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
    data$density_ug_L <- data$cw * 1e6
    data <- data[, c("date", "chemical", "density_ug_L")]
  } else {
    data$density_ug_L <- data$cs * 1e6
    data <- data[, c("date", "chemical", "density_ug_L")]
  }

  plot_df <- data |>
    stats::reshape(idvar = "date",
                   timevar = "chemical",
                   direction = "wide",
                   sep = "."
    ) |>
    xts::as.xts()

  p <- dygraphs::dygraph(plot_df, group = dygraph_group)

  p <- p |>
    dygraphs::dyAxis("y",
                     label = "Density [\u{03BC}g / L]",
                     axisLabelWidth = 80) |>
    dygraphs::dyLegend(showZeroValues = FALSE, labelsSeparateLines = TRUE) |>
    dygraphs::dyRangeSelector() |>
    dygraphs::dyUnzoom()

  return(p)
}
