dss_run_server <- function(id, parameters, run) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    param_hash <- shiny::reactive( digest::digest(lapply(parameters, \(r) r())) )

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

      erahumed_simulation() |>
        setup_from_par_list(parameters$hydrology(), setup_hydrology) |>
        setup_from_par_list(parameters$exposure(), setup_exposure) |>
        setup_from_par_list(parameters$risk(), setup_risk) |>
        run_simulation()
      }) |>
      shiny::bindEvent(run(), ignoreNULL = FALSE)

    return(res)
  })
}

setup_from_par_list <- function(simulation, parameters, setup_fun) {
  args <- c(list(simulation = simulation), parameters)
  do.call(setup_fun, args)
}

