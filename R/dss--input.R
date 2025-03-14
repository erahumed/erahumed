#' @importFrom bslib card card_header card_body card_footer
dss_input_ui <- function(id) {
  ns <- shiny::NS(id)

  tltp <- function(name) {
    bslib::tooltip(shiny_icon("question-circle"),
                   erahumed_input_desc(name, strip_roxy = TRUE),
                   placement = "right")
    }

  date_range_tooltip <- bslib::tooltip(
    shiny_icon("question-circle"),
    "Date range for simulation.",
    placement = "right"
  )

  bslib::page_fillable(
    title = "Input",
    bslib::layout_column_wrap(
      shiny::numericInput(ns("seed"),
                          shiny::p("Seed for simulation", tltp("seed")),
                          value = eval(formals(erahumed_simulation)$seed),
                          step = 1),

      shinyWidgets::airDatepickerInput(inputId = ns("date_range"),
                                       range = TRUE,
                                       label = shiny::p("Date Range", date_range_tooltip),
                                       value = c(as.Date("2020-01-01"), as.Date("2020-12-31"))
      ),

      shiny::div(
        shiny::p(shiny::strong("Rice variety proportions"), tltp("variety_prop")),
        shiny::numericInput(ns("prop_jsendra"),
                            "Proportion of 'J.Sendra'",
                            value = eval(formals(erahumed_simulation)$variety_prop)[[1]],
                            min = 0,
                            max = 10,
                            step = 0.01),
        shiny::numericInput(ns("prop_bomba"),
                            "Proportion of 'Bomba' variety",
                            value = eval(formals(erahumed_simulation)$variety_prop)[[2]],
                            min = 0,
                            max = 10,
                            step = 0.01),
        shiny::numericInput(ns("prop_clearfield"),
                            "Proportion of 'Clearfield' variety",
                            value = eval(formals(erahumed_simulation)$variety_prop)[[3]],
                            min = 0,
                            max = 10,
                            step = 0.01),
        style = "border: 1px solid lightgray; padding: 10px; margin: 5px;",
      ),



      shiny::actionButton(ns("open_outflows_df_modal"), "Setup Outflows DF") |>
        bslib::tooltip(erahumed_input_desc("outflows_df")),
      shiny::actionButton(ns("open_weather_df_modal"), "Setup Weather DF") |>
        bslib::tooltip(erahumed_input_desc("weather_df")),

      shiny::div(
        shiny::p(shiny::strong("Storage curve")),
        shiny::numericInput(ns("sc_intercept"),
                            label = shiny::p(
                              "Intercept [10\u{2076}\u{00B7}m\u{00B3}]",
                              tltp("storage_curve_intercept_m3")),
                            value = eval(formals(erahumed_simulation)$storage_curve_intercept_m3) / 1e6,
                            min = 0,
                            max = 50,
                            step = 0.001),
        shiny::numericInput(ns("sc_slope"),
                            label = shiny::p(
                              "Slope [10\u{2076}\u{00B7}m\u{00B2}]",
                              tltp("storage_curve_slope_m2")),
                            value = eval(formals(erahumed_simulation)$storage_curve_intercept_m3) / 1e6,
                            min = 0,
                            max = 50,
                            step = 0.001)
      ),

      shiny::div(
        shiny::p(shiny::strong("P-ETP function")),
        shiny::numericInput(ns("petp_surface"),
                            label = shiny::p(
                              "PETP-surface [10\u{2076}\u{00B7}m\u{00B2}]",
                              tltp("petp_surface_m2")),
                            value = eval(formals(erahumed_simulation)$petp_surface_m2) / 1e6,
                            min = 0,
                            max = 1000,
                            step = 0.001)
      ),

      shiny::numericInput(ns("ideal_flow_rate_cm"),
                          shiny::p("Ideal Flow Rate [cm]", tltp("ideal_flow_rate_cm")),
                          value = eval(formals(erahumed_simulation)$ideal_flow_rate_cm),
                          min = 0,
                          max = 20,
                          step = 0.5),
      shiny::numericInput(ns("height_thresh_cm"),
                          shiny::p("Height Threshold [cm]", tltp("height_thresh_cm")),
                          value = eval(formals(erahumed_simulation)$height_thresh_cm),
                          min = 0,
                          max = 10,
                          step = 0.1),
      shiny::actionButton(ns("open_management_df_modal"), "Setup Management DF") |>
        bslib::tooltip(erahumed_input_desc("management_df")),
      shiny::numericInput(ns("ditch_level_m"),
                          shiny::p("Ditch Level [m]", tltp("ditch_level_m")),
                          value = eval(formals(erahumed_simulation)$ditch_level_m),
                          min = 0,
                          max = 2,
                          step = 0.01
                          ),
      shiny::actionButton(ns("open_ca_schedules_df_modal"), "Setup Applications DF") |>
        bslib::tooltip(erahumed_input_desc("ca_schedules_df")),

      shiny::numericInput(ns("drift"),
                          shiny::p("drift", tltp("drift")),
                          value = eval(formals(erahumed_simulation)$drift),
                          step = 0.01,
                          min = 0,
                          max = 1),
      shiny::numericInput(ns("covmax"),
                          shiny::p("covmax", tltp("covmax")),
                          value = eval(formals(erahumed_simulation)$covmax),
                          step = 0.01,
                          min = 0,
                          max = 1),
      shiny::numericInput(ns("jgrow"),
                          shiny::p("jgrow", tltp("jgrow")),
                          value = eval(formals(erahumed_simulation)$jgrow),
                          step = 1,
                          min = 0,
                          max = 400),
      shiny::numericInput(ns("SNK"),
                          shiny::p("SNK", tltp("SNK")),
                          value = eval(formals(erahumed_simulation)$SNK),
                          step = 1,
                          min = 0,
                          max = 400),
      shiny::numericInput(ns("dact_m"),
                          shiny::p("dact_m", tltp("dact_m")),
                          value = eval(formals(erahumed_simulation)$dact_m),
                          step = 0.01,
                          min = 0,
                          max = 1),
      shiny::numericInput(ns("css_ppm"),
                          shiny::p("css_ppm", tltp("css_ppm")),
                          value = eval(formals(erahumed_simulation)$css_ppm),
                          step = 1,
                          min = 0,
                          max = 500),
      shiny::numericInput(ns("foc"),
                          shiny::p("foc", tltp("foc")),
                          value = eval(formals(erahumed_simulation)$foc),
                          step = 0.001,
                          min = 0,
                          max = 1),
      shiny::numericInput(ns("bd_g_cm3"),
                          shiny::p("bd_g_cm3", tltp("bd_g_cm3")),
                          value = eval(formals(erahumed_simulation)$bd_g_cm3),
                          step = 0.01,
                          min = 0,
                          max = 10),
      shiny::numericInput(ns("qseep_m_day"),
                          shiny::p("qseep_m_day", tltp("qseep_m_day")),
                          value = eval(formals(erahumed_simulation)$qseep_m_day),
                          step = 0.01,
                          min = 0,
                          max = 10),
      shiny::numericInput(ns("wilting"),
                          shiny::p("wilting", tltp("wilting")),
                          value = eval(formals(erahumed_simulation)$wilting),
                          step = 0.01,
                          min = 0,
                          max = 1),
      shiny::numericInput(ns("fc"),
                          shiny::p("fc", tltp("fc")),
                          value = eval(formals(erahumed_simulation)$fc),
                          step = 0.01,
                          min = 0,
                          max = 1)

      )
    )

}

dss_input_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    tltp <- function(name) {
      bslib::tooltip(shiny_icon("question-circle"),
                     erahumed_input_desc(name, strip_roxy = TRUE),
                     placement = "right")
      }

    outflows_df <- csvInputServer("outflows", erahumed::albufera_outflows)
    shiny::observeEvent(input$open_outflows_df_modal, {
      shiny::showModal(shiny::modalDialog(
        csvInputUI(
          ns("outflows"),
          columns = erahumed_input_docs("outflows_df", "columns")
        ),
        title = shiny::p("Setup Outflows Dataset", tltp("outflows_df")),
        size = "xl"
      ))
    })

    weather_df <- csvInputServer("weather", erahumed::albufera_weather)
    shiny::observeEvent(input$open_weather_df_modal, {
      shiny::showModal(shiny::modalDialog(
        csvInputUI(
          ns("weather"),
          columns = erahumed_input_docs("weather_df", "columns")
        ),
        title = shiny::p("Setup Weather Dataset", tltp("weather_df")),
        size = "xl"
      ))
    })

    management_df <- csvInputServer("management", erahumed::albufera_management)
    shiny::observeEvent(input$open_management_df_modal, {
      shiny::showModal(shiny::modalDialog(
        csvInputUI(
          ns("management"),
          columns = erahumed_input_docs("management_df", "columns")
        ),
        title = shiny::p("Setup Management Dataset", tltp("management_df")),
        size = "xl"
      ))
    })

    ca_schedules_df <- csvInputServer("applications", erahumed::albufera_ca_schedules)
    shiny::observeEvent(input$open_ca_schedules_df_modal, {
      shiny::showModal(shiny::modalDialog(
        csvInputUI(
          ns("applications"),
          columns = erahumed_input_docs("ca_schedules_df", "columns")
        ),
        title = shiny::p("Setup Applications Dataset", tltp("ca_schedules_df")),
        size = "xl"
      ))
    })

    res <- shiny::reactive(list(
      date_start = input$date_range[1],
      date_end = input$date_range[2],
      outflows_df = outflows_df(),
      weather_df = weather_df(),
      variety_prop =
        c(input$prop_jsendra, input$prop_bomba, input$prop_clearfield),
      storage_curve_slope_m2 = 1e6 * input$sc_slope,
      storage_curve_intercept_m3 = 1e6 * input$sc_intercept,
      petp_surface_m2 = 1e6 * input$petp_surface,
      management_df = management_df(),
      ideal_flow_rate_cm = input$ideal_flow_rate_cm,
      height_thresh_cm = input$height_thresh_cm,
      ditch_level_m = input$ditch_level_m,
      ca_schedules_df = ca_schedules_df(),
      drift = input$drift,
      covmax = input$covmax,
      jgrow = input$jgrow,
      SNK = input$SNK,
      dact_m = input$dact_m,
      css_ppm = input$css_ppm,
      foc = input$foc,
      bd_g_cm3 = input$bd_g_cm3,
      qseep_m_day = input$qseep_m_day,
      wilting = input$wilting,
      fc = input$fc,
      seed = input$seed
      ))

    return(res)

  })
}

layer_card_header <- function(layer) {
  docs <- erahumed_input_docs("layers", layer)

  title <- docs[["title"]]
  description <- docs[["description"]]

  card_header(title,
              bslib::tooltip(
                shiny_icon("question-circle"),
                description,
                placement = "right"
              ),
              class = "bg-dark")

}
