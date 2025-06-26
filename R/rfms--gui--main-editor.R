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
    ),

    shiny::actionButton(ns("reset"), label = "Reset all to defaults", icon = shiny::icon("undo")),

    shiny::hr(),

    shiny::h1("Chemical applications"),
    bslib::layout_columns(
      col_widths = c(5, 7),
      applications_db_ui(ns("applications")),
      bslib::card(
        bslib::card_header("Applications timeline"),
        dygraphs::dygraphOutput(ns("timeline_plot")), fill = FALSE)
    )
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
                     Please review or remove this schedule, skipping for now."),
              type = "warning"
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

    output$summary_output <- shiny::renderPrint({
      if (is.null(res())) {
        cat("Invalid configuration. Please check the input values.")
      } else {
        summary(res())
      }
    })

    output$timeline_plot <- dygraphs::renderDygraph({
      shiny::req(res())
      plot_rfms(res())
    })

    return(res)
  })
}
