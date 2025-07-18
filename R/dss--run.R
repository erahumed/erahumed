dss_run_server <- function(id, parameters, run) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    res <- shiny::reactiveVal(NULL)

    # Show "not yet run" overlay on startup
    shinyjs::show("initial_overlay")
    shinyjs::hide("running_overlay")

    prog_msg_reactive <- shiny::reactiveVal("Starting simulation...")
    output$progress_message <- shiny::renderText(prog_msg_reactive())

    shiny::observe({
      shinyjs::hide("initial_overlay")
      shinyjs::show("running_overlay")

      shiny::withProgress(message = "Running simulation...", value = 0, {
        update_msg <- function(msg, step) {
          incProgress(amount = step, detail = msg)
          prog_msg_reactive(msg)  # update message too, if you still want to show it in the UI
        }

        args <- c(parameters(), list(.progress = \(msg) update_msg(msg, 1/10)) )


        tryCatch({
          res( do.call(erahumed_simulation, args) )
        },
        error = function(e) {
          shiny::showNotification(paste("Simulation error:", e$message), type = "error")
          cat("Simulation error: ", e$message)

          shiny::req(FALSE)
        },
        finally = {
          shiny::removeNotification(id = ns("rerun_notif"))
          shinyjs::hide("running_overlay")
          if (is.null(res()))
            shinyjs::show("initial_overlay")
        }
        )
      })

    }) |>
      shiny::bindEvent(run(), ignoreNULL = TRUE, ignoreInit = TRUE)


    shiny::observe({
      shiny::showNotification(
        "Simulation parameters have changed. Click 'Run simulation' to update results.",
        duration = NULL,
        type = "warning",
        id = ns("rerun_notif")
      )
    }) |>
      shiny::bindEvent(parameters(), ignoreInit = TRUE)

    return(res)
  })
}


