dss_input_seed <- function(id) {
  shiny::numericInput(
    inputId = id,
    label = shiny::p("Simulation random seed", dss_input_tooltip("seed")),
    value = dss_input_defaults()[["seed"]],
    step = 1
    )
}

dss_input_date_range <- function(id, date_min = NULL, date_max = NULL) {
  tltp <- bslib::tooltip(trigger = shiny_icon("question-circle"),
                         "Date range for simulation.",
                         placement = "right")

  shinyWidgets::airDatepickerInput(
    inputId = id,
    label = shiny::p("Date Range", tltp),
    range = TRUE,
    value = dss_input_defaults()[["date_range"]],
    minDate = date_min,
    maxDate = date_max
    )
}

dss_input_sc_intercept <- function(id) {
  shiny::numericInput(
    inputId = id,
    label = shiny::p("Intercept [10\u{2076}\u{00B7}m\u{00B3}]", dss_input_tooltip("storage_curve_intercept_m3")),
    value = dss_input_defaults()[["sc_intercept"]],
    min = 0,
    max = 50,
    step = 0.001
    )
}

dss_input_sc_slope <- function(id) {
  shiny::numericInput(
    inputId = id,
    label = shiny::p("Slope [10\u{2076}\u{00B7}m\u{00B2}]", dss_input_tooltip("storage_curve_slope_m2")),
    value = dss_input_defaults()[["sc_slope"]],
    min = 0,
    max = 100,
    step = 0.001
    )
}

dss_input_petp_surface <- function(id) {
  shiny::numericInput(
    inputId = id,
    label = shiny::p("PETP-surface [10\u{2076}\u{00B7}m\u{00B2}]", dss_input_tooltip("petp_surface_m2")),
    value = dss_input_defaults()[["petp_surface"]],
    min = 0,
    max = 1000,
    step = 0.001
    )
}

dss_input_ideal_flow_rate_cm <- function(id) {
  shiny::numericInput(
    inputId = id,
    label = dss_input_label("ideal_flow_rate_cm"),
    value = dss_input_defaults()[["ideal_flow_rate_cm"]],
    min = 0,
    max = 20,
    step = 0.5
    )
}

dss_input_height_thresh_cm <- function(id) {
  shiny::numericInput(
    inputId = id,
    label = dss_input_label("height_thresh_cm"),
    value = dss_input_defaults()[["height_thresh_cm"]],
    min = 0,
    max = 10,
    step = 0.1
    )
}

dss_input_ditch_level_m <- function(id) {
  shiny::numericInput(
    inputId = id,
    label = dss_input_label("ditch_level_m"),
    value = dss_input_defaults()[["ditch_level_m"]],
    min = 0,
    max = 2,
    step = 0.01
  )
}

dss_input_covmax <- function(id) {
  shiny::numericInput(
    inputId = id,
    label = dss_input_label("covmax"),
    value = dss_input_defaults()[["covmax"]],
    step = 0.01,
    min = 0,
    max = 1
  )
}

dss_input_jgrow <- function(id) {
  shiny::numericInput(
    inputId = id,
    label = dss_input_label("jgrow"),
    value = dss_input_defaults()[["jgrow"]],
    step = 1,
    min = 0,
    max = 400
  )
}

dss_input_dact_m <- function(id) {
  shiny::numericInput(
    inputId = id,
    label = dss_input_label("dact_m"),
    value = dss_input_defaults()[["dact_m"]],
    step = 0.01,
    min = 0,
    max = 1
  )
}

dss_input_css_ppm <- function(id) {
  shiny::numericInput(
    inputId = id,
    label = dss_input_label("css_ppm"),
    value = dss_input_defaults()[["css_ppm"]],
    step = 1,
    min = 0,
    max = 500
  )
}

dss_input_foc_ss <- function(id) {
  shiny::numericInput(
    inputId = id,
    label = dss_input_label("foc_ss"),
    value = dss_input_defaults()[["foc_ss"]],
    step = 0.001,
    min = 0,
    max = 1
  )
}

dss_input_foc_sed <- function(id) {
  shiny::numericInput(
    inputId = id,
    label = dss_input_label("foc_sed"),
    value = dss_input_defaults()[["foc_sed"]],
    step = 0.001,
    min = 0,
    max = 1
  )
}

dss_input_bd_g_cm3 <- function(id) {
  shiny::numericInput(
    inputId = id,
    label = dss_input_label("bd_g_cm3"),
    value = dss_input_defaults()[["bd_g_cm3"]],
    step = 0.01,
    min = 0,
    max = 10
  )
}

dss_input_porosity <- function(id) {
  shiny::numericInput(
    inputId = id,
    label = dss_input_label("porosity"),
    value = dss_input_defaults()[["porosity"]],
    step = 0.01,
    min = 0,
    max = 1
  )
}

dss_input_ksetl_m_day <- function(id) {
  shiny::numericInput(
    inputId = id,
    label = dss_input_label("ksetl_m_day"),
    value = dss_input_defaults()[["ksetl_m_day"]],
    step = 0.1,
    min = 0
  )
}

dss_input_weather_df_button <- function(id) {
  btn <- shiny::actionButton(inputId = id, label = "Setup Weather DF")
  bslib::tooltip(trigger = btn, get_param_desc("weather_df", fun = "simulation"))
}

dss_input_outflows_df_button <- function(id) {
  btn <- shiny::actionButton(inputId = id, label = "Setup Outflows DF")
  bslib::tooltip(trigger = btn, get_param_desc("outflows_df", fun = "simulation"))
}

