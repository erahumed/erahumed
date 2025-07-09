dss_run_server <- function(id, parameters, run) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    res <- shiny::reactiveVal(NULL)

    # Show "not yet run" overlay on startup
    shinyjs::show("initial_overlay")
    shinyjs::hide("running_overlay")

    shiny::observe({
      shinyjs::hide("initial_overlay")
      shinyjs::show("running_overlay")

      sim <- run_sim(parameters, ns)
      res(sim)

      shinyjs::hide("running_overlay")
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



run_sim <- function(parameters, ns) {
  tryCatch(parameters(),
    error = function(e) {
      shiny::showNotification(paste("Parameters error:", e$message), type = "error")
      cat("Parameters error: ", e$message)

      shiny::req(FALSE)
  })


  tryCatch(
    do.call(erahumed_simulation, parameters()),
    error = function(e) {
      shiny::showNotification(paste("Simulation error:", e$message), type = "error")
      cat("Simulation error: ", e$message)

      shiny::req(FALSE)
    },
    finally = shiny::removeNotification(id = ns("rerun_notif"))
  )
}
