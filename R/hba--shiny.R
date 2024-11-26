hbaUI <- function(id) {
  ns <- shiny::NS(id)

  vars <- hba_var_labs(invert = TRUE)
  vars <- vars[vars != "level"]  # Easy fix for #101

  shiny::tabsetPanel(

    shiny::tabPanel("Output",

      shiny::fluidRow(
        shiny::column(4, shiny::selectInput(ns("variable"),
                                            "Select Variable",
                                            choices = vars,
                                            selected = vars[[1]])
                      ),
        shiny::column(4,
                      shiny::downloadButton(ns("downloadData"), "Download Data")
                      )
        ),

      plotly::plotlyOutput(ns("plot"))

      ),



      shiny::tabPanel("Setup",
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
                            step = 0.001),
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

hbaServer <- function(id, simulation, shared) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Read setup stuff
    #
    res <- shiny::reactive({
      storage_curve <- function(level) {
        1e6 * input$sc_intercept + 1e6 * input$sc_slope * level
      }

      petp_function <- function(p, etp) {
        1e3 * input$p_surface * p - 1e3 * input$etp_surface * etp
      }

      simulation() |>
        setup_hba(storage_curve = storage_curve, petp_function = petp_function) |>
        run_simulation(layer = "hba")
      })

    output$plot <- plotly::renderPlotly({
      res() |>
        get_layer("hba") |>
        plot(input$variable)
      })

    output$downloadData <- shiny::downloadHandler(
      filename = function() paste0("output-hba-", Sys.Date(), ".csv"),
      content = \(file) readr::write_csv(get_layer_output(res(), "hba"), file)
    )

    return(res)
  })
}
