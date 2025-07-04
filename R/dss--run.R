dss_run_server <- function(id, parameters, run) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    shiny::observe({
      shiny::showNotification(
        "Simulation parameters have changed. Click 'Run simulation' to update results.",
        duration = NULL,
        type = "warning",
        id = ns("rerun_notif")
        )
      }) |>
      shiny::bindEvent(parameters(), ignoreInit = TRUE)



    shiny::reactive(run_sim(parameters, ns)) |>
      shiny::bindEvent(run(), ignoreNULL = TRUE, ignoreInit = TRUE)

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
