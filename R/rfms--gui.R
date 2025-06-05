rfms_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::page_fillable(
    title = "Rice Field Management System Definition",
    shinyjs::useShinyjs(),

    shiny::fluidRow(
      shiny::column(4,
             shiny::numericInput(ns("sowing_yday"),
                                 "Sowing day of year",
                                 value = rfms_input_defaults()$sowing_yday,
                                 min = 1, max = 366
             ),
             shiny::numericInput(ns("perellona_end_yday"),
                                 "Perellona end day of year",
                                 value = rfms_input_defaults()$perellona_end_yday,
                                 min = 1, max = 366
             )
      ),
      shiny::column(4,
             shiny::numericInput(ns("harvesting_yday"),
                                 "Harvesting day of year",
                                 value = rfms_input_defaults()$harvesting_yday,
                                 min = 1, max = 366
             ),
             shiny::numericInput(ns("flow_height_cm"),
                                 "Flow height (cm)",
                                 value = rfms_input_defaults()$flow_height_cm,
                                 min = 0
             )
      ),
      shiny::column(4,
             shiny::numericInput(ns("perellona_start_yday"),
                                 "Perellona start day of year",
                                 value = rfms_input_defaults()$perellona_start_yday,
                                 min = 1, max = 366
             ),
             shiny::numericInput(ns("perellona_height_cm"),
                                 "Perellona height (cm)",
                                 value = rfms_input_defaults()$perellona_height_cm,
                                 min = 0
             )
      )
    ),
    shiny::actionButton(ns("reset"), "Reset to defaults"),

    shiny::hr(),
    shiny::tags$h4("Current system summary:"),
    shiny::verbatimTextOutput(ns("summary_output")),

    shiny::hr(),
    shiny::actionButton(ns("add_chemical"), "Schedule application (coming soon)"),
    shiny::tags$h4("Defined chemicals:"),
    shiny::uiOutput(ns("db_output"))
  )
}


rfms_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values to hold the management system object
    rv <- shiny::reactiveValues(
      system = new_management_system()
    )

    # Helper: update the system whenever any input changes
    shiny::observe({
      tryCatch({
        rv$system <- new_management_system(
          sowing_yday = input$sowing_yday,
          harvesting_yday = input$harvesting_yday,
          perellona_start_yday = input$perellona_start_yday,
          perellona_end_yday = input$perellona_end_yday,
          flow_height_cm = input$flow_height_cm,
          perellona_height_cm = input$perellona_height_cm
        )
      }, error = function(e) {
        # Avoid system update on validation failure
        rv$system <- NULL
      })
    })

    # Output the summary of the system
    output$summary_output <- shiny::renderPrint({
      if (is.null(rv$system)) {
        cat("Invalid configuration. Please check the input values.")
      } else {
        summary(rv$system)
      }
    })

    # Reset button
    shiny::observe({
      shiny::updateNumericInput(session, "sowing_yday", value = rfms_input_defaults()$sowing_yday)
      shiny::updateNumericInput(session, "harvesting_yday", value = rfms_input_defaults()$harvesting_yday)
      shiny::updateNumericInput(session, "perellona_start_yday", value = rfms_input_defaults()$perellona_start_yday)
      shiny::updateNumericInput(session, "perellona_end_yday", value = rfms_input_defaults()$perellona_end_yday)
      shiny::updateNumericInput(session, "flow_height_cm", value = rfms_input_defaults()$flow_height_cm)
      shiny::updateNumericInput(session, "perellona_height_cm", value = rfms_input_defaults()$perellona_height_cm)
    }) |>
      shiny::bindEvent(input$reset)

    # Placeholder output for application database
    output$db_output <- shiny::renderUI({
      shiny::tags$p("(Application editing not yet implemented.)")
    })

    # Return the current management system object if needed
    return(shiny::reactive(rv$system))
  })
}


rfms_input_defaults <- function() {
  fmls <- formals(new_management_system)
  list(
    sowing_yday = eval(fmls$sowing_yday),
    harvesting_yday = eval(fmls$harvesting_yday),
    perellona_end_yday = eval(fmls$perellona_end_yday),
    perellona_start_yday = eval(fmls$perellona_start_yday),
    flow_height_cm = eval(fmls$flow_height_cm),
    perellona_height_cm = eval(fmls$perellona_height_cm)
  )
}
