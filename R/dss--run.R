dss_run_server <- function(id, parameters, run) {
  shiny::moduleServer(id, function(input, output, session) {

    shiny::reactive({
      erahumed_simulation() |>
        setup_from_par_list(parameters$hydrology(), setup_hydrology) |>
        setup_from_par_list(parameters$exposure(), setup_exposure) |>
        setup_from_par_list(parameters$risk(), setup_risk) |>
        run_simulation()
    }) |>
      shiny::bindEvent(run, ignoreNULL = FALSE)

  })
}

setup_from_par_list <- function(simulation, parameters, setup_fun) {
  args <- c(list(simulation = simulation), parameters)
  do.call(setup_fun, args)
}

