hbl_input_ui <- function(id) {
  ns <- shiny::NS(id)

  tltp <- function(param) param_tooltip(layer = "hbl", param = param)


  shiny::tagList(
    shiny::div(
      shiny::p(shiny::strong("Storage curve")),
      shiny::numericInput(ns("sc_intercept"),
                          label = shiny::p(
                            "Intercept [10\u{2076}\u{00B7}m\u{00B3}]",
                            tltp("storage_curve_intercept_m3")),
                          value = eval(formals(setup_hydrology)$storage_curve_intercept_m3) / 1e6,
                          min = 0,
                          max = 50,
                          step = 0.001),
      shiny::numericInput(ns("sc_slope"),
                          label = shiny::p(
                            "Slope [10\u{2076}\u{00B7}m\u{00B2}]",
                            tltp("storage_curve_slope_m2")),
                          value = eval(formals(setup_hydrology)$storage_curve_intercept_m3) / 1e6,
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
                          value = eval(formals(setup_hydrology)$petp_surface_m2) / 1e6,
                          min = 0,
                          max = 1000,
                          step = 0.001)
    )

  )
}


hbl_input_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::reactive({
      list(
        storage_curve_slope_m2 = 1e6 * input$sc_slope,
        storage_curve_intercept_m3 = 1e6 * input$sc_intercept,
        petp_surface_m2 = 1e6 * input$petp_surface
      )
    })

  })
}
