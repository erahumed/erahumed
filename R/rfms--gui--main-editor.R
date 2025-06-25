rfms_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::page_fillable(
    title = "Rice Field Management System Definition",
    shinyjs::useShinyjs(),

    shiny::fluidRow(

      # Crop calendar
      shiny::column(6,
                    bslib::card(
                      bslib::card_header("Crop calendar"),
                      bslib::card_body(
                        rfms_input_sowing_yday(ns("sowing_yday")),
                        rfms_input_perellona_start_yday(ns("perellona_start_yday")),
                        rfms_input_perellona_end_yday(ns("perellona_end_yday")),
                        rfms_input_harvesting_yday(ns("harvesting_yday"))
                      )
                    )
      ),

      shiny::column(6,
                    bslib::card(
                      bslib::card_header("Water levels"),
                      bslib::card_body(
                        rfms_input_flow_height_cm(ns("flow_height_cm")),
                        rfms_input_perellona_height_cm(ns("perellona_height_cm"))
                      )
                    )
      )


    )
    ,
    shiny::actionButton(ns("reset"), label = "Reset all to defaults", icon = shiny::icon("undo")),

    shiny::hr(),
    shiny::tags$h4("Current system summary:"),
    shiny::verbatimTextOutput(ns("summary_output")),

    shiny::hr(),
    applications_db_ui(ns("applications"))
  )
}


rfms_server <- function(id, chemical_db, initial_rfms) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    assert_management_system(initial_rfms)

    # Reset inputs to defaults
    shiny::observe({
      shiny::updateNumericInput(session, "sowing_yday", value = initial_rfms$sowing_yday)
      shiny::updateNumericInput(session, "harvesting_yday", value = initial_rfms$harvesting_yday)
      shiny::updateNumericInput(session, "perellona_start_yday", value = initial_rfms$perellona_start_yday)
      shiny::updateNumericInput(session, "perellona_end_yday", value = initial_rfms$perellona_end_yday)
      shiny::updateNumericInput(session, "flow_height_cm", value = initial_rfms$flow_height_cm)
      shiny::updateNumericInput(session, "perellona_height_cm", value = initial_rfms$perellona_height_cm)
    }) |> shiny::bindEvent(input$reset)

    applications_db <- applications_db_server(
      "applications",
      chemical_db = chemical_db,
      harvesting_yday = shiny::reactive(input$harvesting_yday),
      sowing_yday = shiny::reactive(input$sowing_yday),
      default_items = shiny::isolate(get_proto_applications(initial_rfms, chemical_db()))
    )

    # Update management system object on input change
    res <- shiny::reactive({
      tryCatch({
        sys <- new_management_system(sowing_yday = input$sowing_yday,
                                     harvesting_yday = input$harvesting_yday,
                                     perellona_start_yday = input$perellona_start_yday,
                                     perellona_end_yday = input$perellona_end_yday,
                                     flow_height_cm = input$flow_height_cm,
                                     perellona_height_cm = input$perellona_height_cm
                                     )

        for (app in applications_db()$items) {
          chemical_index <- match(app$chemical_id, chemical_db()$ids)
          if (is.na(chemical_index)) {
            shiny::showNotification(
              paste("Definition of", app$chemical_name,
                    "application on seed day", app$seed_day,
                    "is inconsistent, due to changes in the chemical database.
                     Please review or remove this schedule, skipping for now.",
              type = "warning")
              )
            next
          }

          app$chemical <- chemical_db()$items[[chemical_index]]

          sys <- sys |>
            schedule_application(
              chemical = app$chemical,
              amount_kg_ha = app$amount_kg_ha,
              seed_day = app$seed_day,
              type = app$type,
              emptying_days = app$emptying_days
            )
          }

        sys
        },
        error = function(e) {
          shiny::showNotification(paste("Error in RFMS definition:", e$message), type = "error")
          cat(e$message)
          shiny::req(FALSE)
      })
    })
    # Output the summary of the system
    output$summary_output <- shiny::renderPrint({
      if (is.null(res())) {
        cat("Invalid configuration. Please check the input values.")
      } else {
        summary(res())
      }
    })

    return(res)
  })


}

applications_db_ui <- function(id) {
  ns <- shiny::NS(id)
  list_manager_ui(
    ns("apps"),
    object_name = "Application",
    list_description = "list"
  )
}

applications_db_server <- function(id, chemical_db, harvesting_yday, sowing_yday, default_items = list()) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    list_manager_server(
      "apps",
      item_editor_ui =
        make_application_editor_ui(chemical_db = chemical_db,
                                   harvesting_yday = harvesting_yday,
                                   sowing_yday = sowing_yday),
      item_editor_server =
        make_application_editor_server(chemical_db = chemical_db),
      item_display_function =
        \(x) paste0(x$chemical_name, " - Seed day: ", x$seed_day),
      default_items = default_items
    )
  })
}

make_application_editor_ui <- function(chemical_db, harvesting_yday, sowing_yday) {
  function(id) {
    ns <- shiny::NS(id)

    choices <- chemical_db()$ids
    names(choices) <- sapply(chemical_db()$items, \(.) .$display_name)

    shiny::tagList(
      shiny::selectInput(ns("chemical_id"), "Select chemical", choices = choices),

      shiny::numericInput(ns("seed_day"),
                          "Application day (since sowing)",
                          value = NA,
                          min = 1,
                          max = harvesting_yday() - sowing_yday()),

      shiny::numericInput(ns("amount_kg_ha"),
                          "Amount (kg/ha)",
                          value = NA,
                          min = 0),

      shiny::selectInput(ns("type"),
                         label = "Type",
                         choices = c("ground", "aerial"),
                         selected = NA),

      shiny::numericInput(ns("emptying_days"),
                          "Emptying days",
                          value = NA,
                          min = 1,
                          step = 1)
    )
  }
}

make_application_editor_server <- function(chemical_db) {
  function(id, item = shiny::reactive(NULL)) {
    shiny::moduleServer(id, function(input, output, session) {
      shiny::observe({
        shiny::req(item())

        shiny::updateSelectInput(inputId = "chemical_id", selected = item()$chemical_id)
        shiny::updateNumericInput(inputId = "seed_day", value = item()$seed_day)
        shiny::updateNumericInput(inputId = "amount_kg_ha", value = item()$amount_kg_ha)
        shiny::updateSelectInput(inputId = "type", selected = item()$type)
        shiny::updateNumericInput(inputId = "emptying_days", value = item()$emptying_days)
      })

      shiny::reactive({
        chemical_idx <- match(input$chemical_id, chemical_db()$ids)
        chemical_name <- chemical_db()$items[[chemical_idx]]$display_name

        list(
          chemical_name = chemical_name,
          chemical_id = input$chemical_id,
          amount_kg_ha = input$amount_kg_ha,
          seed_day = input$seed_day,
          type = input$type,
          emptying_days = input$emptying_days
        )
      })
    })
  }
}

get_proto_applications <- function(rfms, chemical_db) {
  lapply(rfms$applications, function(app) {
    list(
      chemical_name = app$chemical$display_name,
      chemical_id = chemical_db$ids[match_chemical(app$chemical, chemical_db$items)],
      amount_kg_ha = app$amount_kg_ha,
      seed_day = app$seed_day,
      type = app$type,
      emptying_days = app$emptying_days
    )
  } )
}
