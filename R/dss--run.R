dss_run_server <- function(id, parameters, run) {
  shiny::moduleServer(id, function(input, output, session) {

    layers <- shiny::reactiveValues()

    shiny::observe({
      sim <- erahumed_simulation() |>
        setup_from_par_list(parameters$inp(), setup_inp) |>
        setup_from_par_list(parameters$hbl(), setup_hbl) |>
        setup_from_par_list(parameters$hbc(), setup_hbc) |>
        setup_from_par_list(parameters$hbd(), setup_hbd) |>
        setup_from_par_list(parameters$ca(), setup_ca) |>
        setup_from_par_list(parameters$ctc(), setup_ctc) |>
        run_simulation()

      layers$inp <- get_layer(sim, "inp")
      layers$hbl <- get_layer(sim, "hbl")
      layers$hbc <- get_layer(sim, "hbc")
      layers$hbd <- get_layer(sim, "hbd")
      layers$ca <- get_layer(sim, "ca")
      layers$ctc <- get_layer(sim, "ctc")

    }) |>
      shiny::bindEvent(run, ignoreNULL = FALSE)

    return( layers )

  })
}

setup_from_par_list <- function(simulation, parameters, setup_fun) {
  args <- c(list(simulation = simulation), parameters)
  do.call(setup_fun, args)
}

