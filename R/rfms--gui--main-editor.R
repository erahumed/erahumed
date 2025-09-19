rfms_ui <- function(id) {
  ns <- shiny::NS(id)

  water_levels_card <- bslib::card(
    bslib::card_header("Water levels"),
    bslib::card_body(
      rfms_input_flow_height_cm(ns("flow_height_cm")),
      rfms_input_perellona_height_cm(ns("perellona_height_cm"))
    ))

  crop_calendar_card <- bslib::card(
    bslib::card_header("Crop calendar"),
    bslib::card_body(rfms_input_crop_calendar(ns("crop_calendar")))
    )

  basic_settings <- bslib::layout_column_wrap(crop_calendar_card, water_levels_card)

  applications_card <-
    bslib::card(
      bslib::card_header(shiny::h4("Chemical Applications")),
      bslib::card_body(
        dygraphs::dygraphOutput(ns("timeline_plot")),
        applications_db_ui(ns("applications"))
      ),
      fill = FALSE
    )


  bslib::page_fillable(shinyjs::useShinyjs(), basic_settings, applications_card)
}


rfms_server <- function(id, chemical_db, initial_rfms) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    assert_rfms(initial_rfms)

    # Reset inputs to defaults
    shiny::observe({
      shinyWidgets::updateNoUiSliderInput(
        session = session,
        inputId = "crop_calendar",
        value = c(initial_rfms$perellona_end_yday,
                  initial_rfms$sowing_yday,
                  initial_rfms$harvesting_yday,
                  initial_rfms$perellona_start_yday)
        )
      shiny::updateNumericInput(session, "flow_height_cm", value = initial_rfms$flow_height_cm)
      shiny::updateNumericInput(session, "perellona_height_cm", value = initial_rfms$perellona_height_cm)
    }) |> shiny::bindEvent(input$reset)

    applications_db <- applications_db_server(
      "applications",
      chemical_db = chemical_db,
      harvesting_yday = shiny::reactive(input$crop_calendar[[3]]),
      sowing_yday = shiny::reactive(input$crop_calendar[[2]]),
      default_items = shiny::isolate(get_proto_applications(initial_rfms, chemical_db()))
    )

    # Update management system object on input change
    res <- shiny::reactive({
      shiny::req(input$crop_calendar, input$flow_height_cm, input$perellona_height_cm)

      tryCatch({
        sys <- new_rfms(sowing_yday = input$crop_calendar[[2]],
                        harvesting_yday = input$crop_calendar[[3]],
                        perellona_start_yday = input$crop_calendar[[4]],
                        perellona_end_yday = input$crop_calendar[[1]],
                        flow_height_cm = input$flow_height_cm,
                        perellona_height_cm = input$perellona_height_cm,
                        display_name = initial_rfms$display_name
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
