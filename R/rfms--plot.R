plot_rfms <- function(x, main = NULL, ...) {
  year <- 2020

  # Hydrological profile
  wms_obj <- wms_from_rfms(x)

  # Create daily dates (leap year assumed)
  dates <- seq(as.Date(paste0(year, "-01-01")),
               as.Date(paste0(year, "-12-31")),
               by = "1 day")

  # Extract water levels
  wms_tancat <- wms_obj[wms_obj$tancat == TRUE, ]
  wms_regular <- wms_obj[wms_obj$tancat == FALSE, ]

  stopifnot(nrow(wms_tancat) == 366, nrow(wms_regular) == 366)

  # Build xts time series
  wms_xts <- xts::xts(
    cbind(
      Regular = wms_regular$ideal_height_eod_cm,
      Tancat = wms_tancat$ideal_height_eod_cm
    ),
    order.by = dates
  )

  # Chemical application events (if any)
  chemical_events <- list()
  if (length(x$applications) > 0) {
    apps_by_day <- split(x$applications, sapply(x$applications, function(app) app$seed_day))

    chemical_events <- lapply(names(apps_by_day), function(seed_day_chr) {
      seed_day <- as.integer(seed_day_chr)
      app_list <- apps_by_day[[seed_day_chr]]
      app_date <- as.Date(paste0(year, "-01-01")) + x$sowing_yday + seed_day - 1

      label <- paste(sapply(app_list, function(app) app$chemical$display_name), collapse = ", ")
      tooltip <- paste(sapply(app_list, function(app) {
        paste0(app$chemical$display_name, ": ",
               app$amount_kg_ha, " kg/ha, ",
               app$type)
      }), collapse = "\n")

      list(date = app_date, label = label, tooltip = tooltip)
    })
  }

  # Define season markers
  season_events <- list(
    list(date = as.Date(paste0(year, "-01-01")) + x$sowing_yday - 1, label = "Sowing start"),
    list(date = as.Date(paste0(year, "-01-01")) + x$harvesting_yday - 1, label = "Sowing end"),
    list(date = as.Date(paste0(year, "-01-01")) + x$perellona_start_yday - 2, label = "Perellon\u00e0 start"),
    list(date = as.Date(paste0(year, "-01-01")) + x$perellona_end_yday, label = "Perellon\u00e0 end")
  )

  # JavaScript axis formatter
  axis_fmt <- "
    function(d) {
       const months = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                       'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];
       return months[d.getMonth()];
     }
  "

  # Build dygraph
  graph <- dygraphs::dygraph(wms_xts, main = main) |>
    dygraphs::dySeries("Regular", label = "Regular cluster", fillGraph = TRUE) |>
    dygraphs::dySeries("Tancat", label = "Tancat cluster", fillGraph = TRUE) |>
    dygraphs::dyAxis("x", axisLabelFormatter = axis_fmt) |>
    dygraphs::dyAxis("y", label = "Water level (cm)") |>
    dygraphs::dyOptions(
      drawGrid = TRUE,
      strokeWidth = 2,
      colors = c("steelblue", "lightblue")
    ) |>
    dygraphs::dyLegend(show = "never") |>
    dygraphs::dyUnzoom()

  # Add chemical application events
  for (ev in chemical_events) {
    graph <- dygraphs::dyEvent(graph, ev$date, ev$label, labelLoc = "top")
  }

  # Add season markers (with custom styling)
  for (ev in season_events) {
    graph <- dygraphs::dyEvent(
      graph, ev$date, ev$label,
      labelLoc = "top",
      color = "darkred",
      strokePattern = "solid"
    )
  }

  graph
}
