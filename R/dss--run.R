dss_run_server <- function(id, parameters, run) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    param_hash <- shiny::reactive( digest::digest(parameters()) )

    shiny::observe({
      shiny::showNotification(
        "Simulation parameters have changed. Click 'Run simulation' to update results.",
        duration = NULL,
        type = "warning",
        id = ns("rerun_notif")
        )
      }) |>
      shiny::bindEvent(param_hash(), ignoreInit = TRUE)

    res <- shiny::reactive({
      shiny::removeNotification(id = ns("rerun_notif"))
      do.call(erahumed_simulation, parameters())
      }) |>
      shiny::bindEvent(run(), ignoreNULL = FALSE)

    return(res)
  })
}
