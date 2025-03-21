#' @importFrom bslib card card_header card_body card_footer
dss_input_ui <- function(id) {
  ns <- shiny::NS(id)

  simulation_parameters_ui <- bslib::layout_column_wrap(
    dss_input_seed(ns("seed")),
    dss_input_date_range(ns("date_range"))
    )

  meteorology_parameters_ui <- bslib::layout_column_wrap(
    dss_input_weather_df_button(ns("open_weather_df_modal"))
    )

  hydrology_parameters_ui <- bslib::layout_column_wrap(
    dss_input_outflows_df_button(ns("open_outflows_df_modal")),
    shiny::div(
      shiny::p(shiny::strong("Water storage parameters")),
      dss_input_sc_intercept(ns("sc_intercept")),
      dss_input_sc_slope(ns("sc_slope")),
      dss_input_petp_surface(ns("petp_surface"))
      ),
    dss_input_ideal_flow_rate_cm(ns("ideal_flow_rate_cm")),
    dss_input_height_thresh_cm(ns("height_thresh_cm")),
    dss_input_ditch_level_m(ns("ditch_level_m")),
    dss_input_management_df_button(ns("open_management_df_modal"))
  )

  crop_parameters_ui <- bslib::layout_column_wrap(
    dss_input_prop_variety_slider(ns("prop_variety_12"))
  )

  pesticide_parameters_ui <- bslib::layout_column_wrap(
    dss_input_ca_schedules_df_button(ns("open_ca_schedules_df_modal"))
    )

  physchem_parameters_ui <- bslib::layout_column_wrap(
    dss_input_drift(ns("drift")),
    dss_input_covmax(ns("covmax")),
    dss_input_jgrow(ns("jgrow")),
    dss_input_SNK(ns("SNK")),
    dss_input_dact_m(ns("dact_m")),
    dss_input_css_ppm(ns("css_ppm")),
    dss_input_foc(ns("foc")),
    dss_input_bd_g_cm3(ns("bd_g_cm3")),
    dss_input_qseep_m_day(ns("qseep_m_day")),
    dss_input_wilting(ns("wilting")),
    dss_input_fc(ns("fc"))
    )

  bslib::page_fillable(
    title = "Input",
    bslib::accordion(
      bslib::accordion_panel("Simulation settings", simulation_parameters_ui),
      bslib::accordion_panel("Hydrology", hydrology_parameters_ui),
      bslib::accordion_panel("Crop parameters", crop_parameters_ui),
      bslib::accordion_panel("Meteorology", meteorology_parameters_ui),
      bslib::accordion_panel("Pesticide parameters", pesticide_parameters_ui),
      bslib::accordion_panel("Physico-chemical parameters", physchem_parameters_ui)
      )
    )
}

dss_input_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    outflows_df <- csvInputServer("outflows", erahumed::albufera_outflows)
    shiny::observeEvent(input$open_outflows_df_modal, {
      shiny::showModal(shiny::modalDialog(
        csvInputUI(
          ns("outflows"),
          columns = erahumed_input_docs("outflows_df", "columns")
        ),
        title = shiny::p("Setup Outflows Dataset", dss_input_tooltip("outflows_df")),
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
        title = shiny::p("Setup Weather Dataset", dss_input_tooltip("weather_df")),
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
        title = shiny::p("Setup Management Dataset", dss_input_tooltip("management_df")),
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
        title = shiny::p("Setup Applications Dataset", dss_input_tooltip("ca_schedules_df")),
        size = "xl"
      ))
    })

    variety_prop <- shiny::reactive({
      c(input$prop_variety_12[[1]],
        input$prop_variety_12[[2]] - input$prop_variety_12[[1]],
        1 - input$prop_variety_12[[2]]
        )
    })

    res <- shiny::reactive({
      list(
        date_start = input$date_range[[1]],
        date_end = input$date_range[[2]],
        outflows_df = outflows_df(),
        weather_df = weather_df(),
        variety_prop = variety_prop(),
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
      )}
    )

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
