plot_rfms <- function(x, ...) {
  year <- 2020

  n_app <- length(x$applications)
  if (n_app == 0) {
    warning("No chemical applications defined.")
    return(invisible(NULL))
  }

  # Create a daily sequence for the full year (used to build dummy xts)
  dates <- seq(as.Date(paste0(year, "-01-01")), as.Date(paste0(year, "-12-31")), by = "1 day")
  dummy_ts <- xts::xts(rep(0, length(dates)), order.by = dates)

  # Group applications by seed_day
  apps_by_day <- split(x$applications, sapply(x$applications, function(app) app$seed_day))

  # Create grouped event entries
  events <- lapply(names(apps_by_day), function(seed_day_chr) {
    seed_day <- as.integer(seed_day_chr)
    app_list <- apps_by_day[[seed_day_chr]]
    app_date <- as.Date(paste0(year, "-01-01")) + x$sowing_yday + seed_day

    label <- paste(sapply(app_list, function(app) app$chemical$display_name), collapse = ", ")
    tooltip <- paste(sapply(app_list, function(app) {
      paste0(app$chemical$display_name, ": ",
             app$amount_kg_ha, " kg/ha, ",
             app$type)
    }), collapse = "\n")

    list(date = app_date, label = label, tooltip = tooltip)
  })

  axis_fmt <- "
    function(d) {
       const months = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                       'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];
       return months[d.getMonth()];
     }
    "

  # Initialize dygraph
  graph <- dygraphs::dygraph(dummy_ts, main = "Chemical Applications Timeline") |>
    dygraphs::dyAxis("x", axisLabelFormatter = axis_fmt) |>
    dygraphs::dyAxis("y", valueRange = c(0, 1)) |>
    dygraphs::dyLegend(show = "never") |>
    dygraphs::dyOptions(drawYAxis = FALSE, drawGrid = TRUE)

  # Add events
  for (ev in events) {
    graph <- dygraphs::dyEvent(graph, ev$date, ev$label, labelLoc = "top")
  }

  graph
}

