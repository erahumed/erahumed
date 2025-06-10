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
    list_manager_ui(ns("applications"),
                    object_name = "Applications",
                    list_description = "list")
  )
}


rfms_server <- function(id, chemical_db) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    system <- shiny::reactiveVal(new_management_system())

    # Update management system object on input change
    shiny::observe({
      tryCatch({
        system (
          new_management_system(sowing_yday = input$sowing_yday,
                                harvesting_yday = input$harvesting_yday,
                                perellona_start_yday = input$perellona_start_yday,
                                perellona_end_yday = input$perellona_end_yday,
                                flow_height_cm = input$flow_height_cm,
                                perellona_height_cm = input$perellona_height_cm
                                )
        )
      }, error = function(e) {
        system(NULL)
      })
    })

    # Output the summary of the system
    output$summary_output <- shiny::renderPrint({
      if (is.null(system())) {
        cat("Invalid configuration. Please check the input values.")
      } else {
        summary(system())
      }
    })

    # Reset inputs to defaults
    shiny::observe({
      shiny::updateNumericInput(session, "sowing_yday", value = rfms_input_defaults()$sowing_yday)
      shiny::updateNumericInput(session, "harvesting_yday", value = rfms_input_defaults()$harvesting_yday)
      shiny::updateNumericInput(session, "perellona_start_yday", value = rfms_input_defaults()$perellona_start_yday)
      shiny::updateNumericInput(session, "perellona_end_yday", value = rfms_input_defaults()$perellona_end_yday)
      shiny::updateNumericInput(session, "flow_height_cm", value = rfms_input_defaults()$flow_height_cm)
      shiny::updateNumericInput(session, "perellona_height_cm", value = rfms_input_defaults()$perellona_height_cm)
    }) |> shiny::bindEvent(input$reset)

    applications_db <- list_manager_server(
      "applications",
      item_editor_ui = function(id, item = NULL) {
        ns <- shiny::NS(id)

        choices <- seq_along(chemical_db())
        names(choices) <- sapply(chemical_db(), \(.) .$display_name)

        selected_chem <- if (!is.null(item))
          match_chemical(item$chemical, chemical_db())

        shiny::tagList(
          shiny::selectInput(ns("chemical_id"),
                             "Select chemical",
                             choices = choices,
                             selected = selected_chem
                             ),
          shiny::numericInput(ns("seed_day"),
                              "Application day (since sowing)",
                              value = item$seed_day,
                              min = 1,
                              max = input$harvesting_yday - input$sowing_yday
                              ),
          shiny::numericInput(ns("amount_kg_ha"),
                              "Amount (kg/ha)",
                              value = item$amount_kg_ha,
                              min = 0),
          shiny::selectInput(ns("type"),
                             label = "Type",
                             choices = c("ground", "aerial"),
                             selected = item$type
                             ),
          shiny::numericInput(ns("emptying_days"),
                              "Emptying days",
                              value = item$emptying_days,
                              min = 1,
                              step = 1
                              )
        )
      },
      item_editor_server = function(id) {
        shiny::moduleServer(id, function(input, output, session) {
          shiny::reactive({
            chem_id <- as.numeric(input$chemical_id)  # Input is a string

            chemical_application(
              chemical = chemical_db()[[chem_id]],
              amount_kg_ha = input$amount_kg_ha,
              seed_day = input$seed_day,
              type = input$type,
              emptying_days = input$emptying_days
            )
          })
        })
      },
      item_display_function = function(item) {
        paste0(item$chemical$display_name, " - Seed day: ", item$seed_day)
      }
      )


    # Return both the current system and the ability to update the chemical database
    return(system)
  })
}


generate_application_schedule_db_ui <- function(chemical_db, sowing_yday, harvesting_yday)
  function(id, item = NULL) {
    ns <- shiny::NS(id)

    choices <- seq_along(chemical_db)
    names(choices) <- sapply(chemical_db, \(.) .[["display_name"]])

    shiny::tagList(
      shiny::selectInput(ns("chemical"),
                         "Select chemical",
                         choices = choices,
                         multiple = TRUE),
      shiny::numericInput(
        ns("seed_day"), "Application day (since sowing)",
        value = 1,
        min = 1,
        max = harvesting_yday - sowing_yday),
      shiny::numericInput(ns("amount_kg_ha"), "Amount (kg/ha)", value = 1, min = 0),
      shiny::selectInput(ns("type"), label = "Type", choices = c("ground", "aerial")),
      shiny::numericInput(ns("emptying_days"), "Emptying days", value = 1, min = 1, step = 1)
  )
}

application_schedule_db_server <- function(id, chemical_db) {
  shiny::moduleServer(id, function(input, output, session) {

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
