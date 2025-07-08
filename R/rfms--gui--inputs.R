rfms_input_sowing_yday <- function(id) {
  desc <- "Day of the year marking the start of the sowing season (1-366, assuming a leap year)."
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
  desc <- "Day of the year marking the end of the sowing season (1-366, assuming a leap year)."
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

rfms_input_chemical_id <- function(id, choices) {
  inline_select_input(id, "Select chemical", choices = choices)
}

rfms_input_seed_day <- function(id, max) {
  desc <- "Day after seeding when the chemical application occurs."
  label <- shiny::p(
    "Application day (since sowing)",
    bslib::tooltip(
      trigger = shiny_icon("question-circle"),
      desc,
      placement = "right"
    )
  )

  inline_numeric_input(id,
                       label,
                       value = NA,
                       min = 1,
                       max = max)
}

rfms_input_amount_kg_ha <- function(id) {
  desc <- "Application rate of chemical active ingredient in kilograms per hectare."
  label <- shiny::p(
    "Dose (kg/ha)",
    bslib::tooltip(
      trigger = shiny_icon("question-circle"),
      desc,
      placement = "right"
    )
  )

  inline_numeric_input(id,
                       label,
                       value = NA,
                       min = 0)
}

rfms_input_type <- function(id) {
  desc <- "Application method: either ground-based or aerial spraying."
  label <- shiny::p(
    "Type",
    bslib::tooltip(
      trigger = shiny_icon("question-circle"),
      desc,
      placement = "right"
    )
  )

  inline_select_input(id,
                      label = label,
                      choices = c("ground", "aerial"),
                      selected = NA)
}

rfms_input_emptying_days <- function(id) {
  desc <- "Number of days the field remains empty after a ground application. Ignored if application type is aerial."
  label <- shiny::p(
    "Emptying days",
    bslib::tooltip(
      trigger = shiny_icon("question-circle"),
      desc,
      placement = "right"
    )
  )

  inline_numeric_input(id,
                       label,
                       value = NA,
                       min = 1,
                       step = 1)
}
