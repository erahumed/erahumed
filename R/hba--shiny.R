hba_input_ui <- function(id) {
  ns <- shiny::NS(id)

  tltp <- function(param) param_tooltip(layer = "hba", param = param)


  shiny::tagList(
    shiny::div(
      shiny::p(shiny::strong("Storage curve"), tltp("storage_curve")),
      shiny::numericInput(ns("sc_intercept"),
                          label = "Storage Curve Intercept [10\u{2076}\u{00B7}m\u{00B3}]",
                          value = 16.7459,
                          min = 0,
                          max = 50,
                          step = 0.001),
      shiny::numericInput(ns("sc_slope"),
                          label = "Storage Curve Slope [10\u{2076}\u{00B7}m\u{00B2}]",
                          value = 23.6577,
                          min = 0,
                          max = 50,
                          step = 0.001)
      ),

    shiny::div(
      shiny::p(shiny::strong("P-ETP function"), tltp("petp_function")),
      shiny::numericInput(ns("p_surface"),
                          label = "P-surface [10\u{2076}\u{00B7}m\u{00B2}]",
                          value = 114.226,
                          min = 0,
                          max = 1000,
                          step = 0.001),
      shiny::numericInput(ns("etp_surface"),
                          label = "ETP-surface [10\u{2076}\u{00B7}m\u{00B2}]",
                          value = 79.361,
                          min = 0,
                          max = 1000,
                          step = 0.001)
    )

  )
}


hba_input_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::reactive({
      list(
        storage_curve = function(level)
          1e6 * input$sc_intercept + 1e6 * input$sc_slope * level,
        petp_function = function(p, etp)
          1e3 * input$p_surface * p - 1e3 * input$etp_surface * etp
      )
    })


  })
}
