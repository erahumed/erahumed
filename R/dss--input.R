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
    dss_input_foc_ss(ns("foc_ss")),
    dss_input_foc_sed(ns("foc_sed")),
    dss_input_bd_g_cm3(ns("bd_g_cm3")),
    dss_input_porosity(ns("porosity")),
    dss_input_ideal_flow_rate_cm(ns("ideal_flow_rate_cm")),
    dss_input_height_thresh_cm(ns("height_thresh_cm")),
    dss_input_ditch_level_m(ns("ditch_level_m")),
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

    outflows_df <- csvInputServer("outflows", erahumed::albufera_outflows)
    shiny::observeEvent(input$open_outflows_df_modal, {
      shiny::showModal(shiny::modalDialog(
        csvInputUI(
          ns("outflows"),
          columns = get_param_docs("outflows_df", fun = "simulation")[["columns"]]
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
          columns = get_param_docs("weather_df", fun = "simulation")[["columns"]]
        ),
        title = shiny::p("Setup Weather Dataset", dss_input_tooltip("weather_df")),
        size = "xl"
      ))
    })

    chemical_db <- chemical_db_server("chemical_db")
    rfms_db <- rfms_db_server("rfms_db", chemical_db)
    rfms_map <- rfcm_server("rfcm", rfms_db = rfms_db, seed = input$seed)

    res <- shiny::reactive({
      shiny::req(input$date_range)
      shiny::req(length(input$date_range) == 2)
      shiny::req(rfms_map())

      list(
        date_start = input$date_range[[1]],
        date_end = input$date_range[[2]],
        rfms_map = rfms_map(),
        outflows_df = outflows_df(),
        weather_df = weather_df(),
        storage_curve_slope_m2 = 1e6 * input$sc_slope,
        storage_curve_intercept_m3 = 1e6 * input$sc_intercept,
        petp_surface_m2 = 1e6 * input$petp_surface,
        ideal_flow_rate_cm = input$ideal_flow_rate_cm,
        height_thresh_cm = input$height_thresh_cm,
        ditch_level_m = input$ditch_level_m,
        covmax = input$covmax,
        jgrow = input$jgrow,
        dact_m = input$dact_m,
        css_ppm = input$css_ppm,
        foc_ss = input$foc_ss,
        foc_sed = input$foc_sed,
        bd_g_cm3 = input$bd_g_cm3,
        porosity = input$porosity,
        seed = input$seed
      )}
    )

    return(res)

  })
}

