rfms_input_sowing_yday <- function(id) {
  desc <- "Day of the year marking the start of the sowing season (1–366, assuming a leap year)."
  label <- shiny::p(
    "Sowing day of year",
    bslib::tooltip(
      trigger = shiny_icon("question-circle"),
      desc,
      placement = "right"
    )
  )

  inline_numeric_input(id,
                       label,
                       value = rfms_input_defaults()$sowing_yday,
                       min = 1,
                       max = 366)
}

rfms_input_perellona_start_yday <- function(id) {
  desc <- "Day of the year marking the beginning of the Perellona flooding period (after harvest)."
  label <- shiny::p(
    "Perellona start day of year",
    bslib::tooltip(
      trigger = shiny_icon("question-circle"),
      desc,
      placement = "right"
    )
  )

  inline_numeric_input(id,
                       label,
                       value = rfms_input_defaults()$perellona_start_yday,
                       min = 1,
                       max = 366)
}

rfms_input_perellona_end_yday <- function(id) {
  desc <- "Day of the year marking the end of the Perellona flooding period (before sowing)."
  label <- shiny::p(
    "Perellona end day of year",
    bslib::tooltip(
      trigger = shiny_icon("question-circle"),
      desc,
      placement = "right"
    )
  )

  inline_numeric_input(id,
                       label,
                       value = rfms_input_defaults()$perellona_end_yday,
                       min = 1,
                       max = 366)
}

rfms_input_harvesting_yday <- function(id) {
  desc <- "Day of the year marking the end of the sowing season (1–366, assuming a leap year)."
  label <- shiny::p(
    "Harvesting day of year",
    bslib::tooltip(
      trigger = shiny_icon("question-circle"),
      desc,
      placement = "right"
    )
  )

  inline_numeric_input(id,
                       label,
                       value = rfms_input_defaults()$harvesting_yday,
                       min = 1,
                       max = 366)
}

rfms_input_flow_height_cm <- function(id) {
  desc <- "Target water level (in cm) during the regular days of the sowing season, excluding emptying and transition days."
  label <- shiny::p(
    "Flow height (cm)",
    bslib::tooltip(
      trigger = shiny_icon("question-circle"),
      desc,
      placement = "right"
    )
  )

  inline_numeric_input(id,
                       label,
                       value = rfms_input_defaults()$flow_height_cm,
                       min = 0)
}

rfms_input_perellona_height_cm <- function(id) {
  desc <- "Target water level (in cm) during the Perellona flooding period."
  label <- shiny::p(
    "Perellona height (cm)",
    bslib::tooltip(
      trigger = shiny_icon("question-circle"),
      desc,
      placement = "right"
    )
  )

  inline_numeric_input(id,
                       label,
                       value = rfms_input_defaults()$perellona_height_cm,
                       min = 0)
}

rfms_input_defaults <- function() {
  fmls <- formals(new_management_system)
  list(
    sowing_yday = eval(fmls$sowing_yday),
    harvesting_yday = eval(fmls$harvesting_yday),
    perellona_end_yday = eval(fmls$perellona_end_yday),
    perellona_start_yday = eval(fmls$perellona_start_yday),
    flow_height_cm = eval(fmls$flow_height_cm),
    perellona_height_cm = eval(fmls$perellona_height_cm)
  )
}
