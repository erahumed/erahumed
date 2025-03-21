dss_input_seed <- function(id) {
  shiny::numericInput(
    inputId = id,
    label = shiny::p("Seed for simulation", dss_input_tooltip("seed")),
    value = eval(formals(erahumed_simulation)$seed),
    step = 1
    )
}

dss_input_date_range <- function(id) {
  tltp <- bslib::tooltip(trigger = shiny_icon("question-circle"),
                         "Date range for simulation.",
                         placement = "right")

  shinyWidgets::airDatepickerInput(
    inputId = id,
    label = shiny::p("Date Range", tltp),
    range = TRUE,
    value = c(
      eval(formals(erahumed_simulation)$date_start),
      eval(formals(erahumed_simulation)$date_end)
      )
    )
}

dss_input_sc_intercept <- function(id) {
  shiny::numericInput(
    inputId = id,
    label = shiny::p("Intercept [10\u{2076}\u{00B7}m\u{00B3}]", dss_input_tooltip("storage_curve_intercept_m3")),
    value = eval(formals(erahumed_simulation)$storage_curve_intercept_m3) / 1e6,
    min = 0,
    max = 50,
    step = 0.001
    )
}

dss_input_sc_slope <- function(id) {
  shiny::numericInput(
    inputId = id,
    label = shiny::p("Slope [10\u{2076}\u{00B7}m\u{00B2}]", dss_input_tooltip("storage_curve_slope_m2")),
    value = eval(formals(erahumed_simulation)$storage_curve_slope_m2) / 1e6,
    min = 0,
    max = 100,
    step = 0.001
    )
}

dss_input_petp_surface <- function(id) {
  shiny::numericInput(
    inputId = id,
    label = shiny::p("PETP-surface [10\u{2076}\u{00B7}m\u{00B2}]", dss_input_tooltip("petp_surface_m2")),
    value = eval(formals(erahumed_simulation)$petp_surface_m2) / 1e6,
    min = 0,
    max = 1000,
    step = 0.001
    )
}

dss_input_ideal_flow_rate_cm <- function(id) {
  shiny::numericInput(
    inputId = id,
    label = shiny::p("Ideal Flow Rate [cm]", dss_input_tooltip("ideal_flow_rate_cm")),
    value = eval(formals(erahumed_simulation)$ideal_flow_rate_cm),
    min = 0,
    max = 20,
    step = 0.5
    )
}

dss_input_height_thresh_cm <- function(id) {
  shiny::numericInput(
    inputId = id,
    label = shiny::p("Height Threshold [cm]", dss_input_tooltip("height_thresh_cm")),
    value = eval(formals(erahumed_simulation)$height_thresh_cm),
    min = 0,
    max = 10,
    step = 0.1
    )
}

dss_input_ditch_level_m <- function(id) {
  shiny::numericInput(
    inputId = id,
    label = shiny::p("Ditch Level [m]", dss_input_tooltip("ditch_level_m")),
    value = eval(formals(erahumed_simulation)$ditch_level_m),
    min = 0,
    max = 2,
    step = 0.01
  )
}

dss_input_drift <- function(id) {
  shiny::numericInput(
    inputId = id,
    label = shiny::p("drift", dss_input_tooltip("drift")),
    value = eval(formals(erahumed_simulation)$drift),
    step = 0.01,
    min = 0,
    max = 1
  )
}

dss_input_covmax <- function(id) {
  shiny::numericInput(
    inputId = id,
    label = shiny::p("covmax", dss_input_tooltip("covmax")),
    value = eval(formals(erahumed_simulation)$covmax),
    step = 0.01,
    min = 0,
    max = 1
  )
}

dss_input_jgrow <- function(id) {
  shiny::numericInput(
    inputId = id,
    label = shiny::p("jgrow", dss_input_tooltip("jgrow")),
    value = eval(formals(erahumed_simulation)$jgrow),
    step = 1,
    min = 0,
    max = 400
  )
}

dss_input_SNK <- function(id) {
  shiny::numericInput(
    inputId = id,
    label = shiny::p("SNK", dss_input_tooltip("SNK")),
    value = eval(formals(erahumed_simulation)$SNK),
    step = 1,
    min = 0,
    max = 400
  )
}

dss_input_dact_m <- function(id) {
  shiny::numericInput(
    inputId = id,
    label = shiny::p("dact_m", dss_input_tooltip("dact_m")),
    value = eval(formals(erahumed_simulation)$dact_m),
    step = 0.01,
    min = 0,
    max = 1
  )
}

dss_input_css_ppm <- function(id) {
  shiny::numericInput(
    inputId = id,
    label = shiny::p("css_ppm", dss_input_tooltip("css_ppm")),
    value = eval(formals(erahumed_simulation)$css_ppm),
    step = 1,
    min = 0,
    max = 500
  )
}

dss_input_foc <- function(id) {
  shiny::numericInput(
    inputId = id,
    label = shiny::p("foc", dss_input_tooltip("foc")),
    value = eval(formals(erahumed_simulation)$foc),
    step = 0.001,
    min = 0,
    max = 1
  )
}

dss_input_bd_g_cm3 <- function(id) {
  shiny::numericInput(
    inputId = id,
    label = shiny::p("bd_g_cm3", dss_input_tooltip("bd_g_cm3")),
    value = eval(formals(erahumed_simulation)$bd_g_cm3),
    step = 0.01,
    min = 0,
    max = 10
  )
}

dss_input_qseep_m_day <- function(id) {
  shiny::numericInput(
    inputId = id,
    label = shiny::p("qseep_m_day", dss_input_tooltip("qseep_m_day")),
    value = eval(formals(erahumed_simulation)$qseep_m_day),
    step = 0.01,
    min = 0,
    max = 10
  )
}

dss_input_wilting <- function(id) {
  shiny::numericInput(
    inputId = id,
    label = shiny::p("wilting", dss_input_tooltip("wilting")),
    value = eval(formals(erahumed_simulation)$wilting),
    step = 0.01,
    min = 0,
    max = 1
  )
}

dss_input_fc <- function(id) {
  shiny::numericInput(
    inputId = id,
    label = shiny::p("fc", dss_input_tooltip("fc")),
    value = eval(formals(erahumed_simulation)$fc),
    step = 0.01,
    min = 0,
    max = 1
  )
}

dss_input_prop_variety_slider <- function(id) {
  vprop_default <- eval(formals(erahumed::erahumed_simulation)$variety_prop)
  value <- c(vprop_default[1], vprop_default[1] + vprop_default[2])
  tltp_txt <- paste0(
    "The two numbers on the slider represent the fraction of the crop surface ",
    "allocated to J. Sendra and the combined area of J. Sendra and Bomba rice ",
    "varieties, respectively."
    )

  tltp <- bslib::tooltip(shiny_icon("question-circle"), tltp_txt, placement = "right")

  shiny::sliderInput(
    inputId = id,
    label = shiny::p("Variety proportion (J.Sendra : Bomba : Clearfield)", tltp),
    min = 0,
    max = 1,
    value = value,
    step = 0.01
    )
}

dss_input_weather_df_button <- function(id) {
  btn <- shiny::actionButton(inputId = id, label = "Setup Weather DF")
  bslib::tooltip(trigger = btn, erahumed_input_desc("weather_df"))
}

dss_input_outflows_df_button <- function(id) {
  btn <- shiny::actionButton(inputId = id, label = "Setup Outflows DF")
  bslib::tooltip(trigger = btn, erahumed_input_desc("outflows_df"))
}

dss_input_management_df_button <- function(id) {
  btn <- shiny::actionButton(inputId = id, label = "Setup Management DF")
  bslib::tooltip(trigger = btn, erahumed_input_desc("management_df"))
}

dss_input_ca_schedules_df_button <- function(id) {
  btn <- shiny::actionButton(inputId = id, label = "Setup Applications DF")
  bslib::tooltip(trigger = btn, erahumed_input_desc("ca_schedules_df"))
}
