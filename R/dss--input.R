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
    card(
      card_header("Lake's hydrology empirical data"),
      dss_input_outflows_df_button(ns("open_outflows_df_modal")),
    ),

    bslib::card(
      bslib::card_header("Lake's water storage parameters"),
      dss_input_sc_intercept(ns("sc_intercept")),
      dss_input_sc_slope(ns("sc_slope")),
      dss_input_petp_surface(ns("petp_surface"))
      )
  )

  physchem_parameters_ui <- bslib::layout_column_wrap(
    dss_input_jgrow(ns("jgrow")),
    dss_input_dact_m(ns("dact_m")),
    dss_input_css_ppm(ns("css_ppm")),
    dss_input_foc(ns("foc")),
    dss_input_bd_g_cm3(ns("bd_g_cm3")),
    dss_input_qseep_m_day(ns("qseep_m_day")),
    dss_input_porosity(ns("porosity")),
    dss_input_ideal_flow_rate_cm(ns("ideal_flow_rate_cm")),
    dss_input_height_thresh_cm(ns("height_thresh_cm")),
    dss_input_ditch_level_m(ns("ditch_level_m")),
    dss_input_drift(ns("drift")),
    dss_input_covmax(ns("covmax"))
    )

  agrochemical_management_ui <- shiny::tagList(
    chemical_db_ui(ns("chemical_db")),
    shiny::hr(),
    rfms_db_ui(ns("rfms_db")),
    shiny::hr(),
    rfcm_ui(ns("rfcm"))
  )

  bslib::page_fillable(
    title = "Input",
    shinyjs::useShinyjs(),
    shiny::tags$div(
      style = "position: fixed; bottom: 20px; left: 20px; z-index: 1000;",
      shiny::actionButton(ns("reset"), "Reset", icon = shiny::icon("undo"),
                          class = "btn-outline-danger btn-lg")
    ),
    bslib::accordion(
      id = ns("input-ui"),
      bslib::accordion_panel("Simulation settings", simulation_parameters_ui),
      bslib::accordion_panel("Hydrology", hydrology_parameters_ui),
      bslib::accordion_panel("Meteorology", meteorology_parameters_ui),
      bslib::accordion_panel("Environmental properties", physchem_parameters_ui),
      bslib::accordion_panel("Agrochemical management", agrochemical_management_ui)
      )
    )
}

dss_input_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    reset_reactive <- shiny::reactiveVal(0)
    shiny::observe( reset_reactive(reset_reactive() + 1) ) |>
      shiny::bindEvent(input$reset)

    outflows_df <- csvInputServer("outflows",
                                  erahumed::albufera_outflows,
                                  reset = reset_reactive)
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

    weather_df <- csvInputServer("weather",
                                 erahumed::albufera_weather,
                                 reset = reset_reactive)
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

    chemical_db <- chemical_db_server("chemical_db")
    rfms_db <- rfms_db_server("rfms_db", chemical_db)
    cluster_map <- rfcm_server("rfcm", rfms_db = rfms_db, seed = input$seed)


    shiny::observe({
      shinyjs::reset("input-ui")
      }) |> shiny::bindEvent(input$reset)

    res <- shiny::reactive({
      shiny::req(input$date_range)
      shiny::req(length(input$date_range) == 2)
      shiny::req(cluster_map())

      list(
        date_start = input$date_range[[1]],
        date_end = input$date_range[[2]],
        cluster_map = cluster_map(),
        outflows_df = outflows_df(),
        weather_df = weather_df(),
        storage_curve_slope_m2 = 1e6 * input$sc_slope,
        storage_curve_intercept_m3 = 1e6 * input$sc_intercept,
        petp_surface_m2 = 1e6 * input$petp_surface,
        ideal_flow_rate_cm = input$ideal_flow_rate_cm,
        height_thresh_cm = input$height_thresh_cm,
        ditch_level_m = input$ditch_level_m,
        drift = input$drift,
        covmax = input$covmax,
        jgrow = input$jgrow,
        dact_m = input$dact_m,
        css_ppm = input$css_ppm,
        foc = input$foc,
        bd_g_cm3 = input$bd_g_cm3,
        qseep_m_day = input$qseep_m_day,
        porosity = input$porosity,
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
