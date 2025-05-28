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

    res <- shiny::reactive({
      shiny::req(parameters())

      tryCatch(
        do.call(erahumed_simulation, parameters()),
        error = function(e) {
          shiny::showNotification(paste("Simulation error:", e$message), type = "error")
          cat(e$message)
          shiny::req(FALSE)
        },
        finally = shiny::removeNotification(id = ns("rerun_notif"))
        )
      }) |>
      shiny::bindEvent(run(), ignoreNULL = FALSE)

    return(res)
  })
}
