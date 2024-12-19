dss_run_server <- function(id, parameters) {
  shiny::moduleServer(id, function(input, output, session) {

    res <- shiny::reactiveValues()

    shiny::observe({
      res$inp <- dss_run_layer(
        layers = list(),
        parameters = parameters$inp(),
        setup_fn = setup_inp,
        get = "inp"
        )
      })

    shiny::observe({
      res$hba <- dss_run_layer(
        layers = list(inp = res$inp),
        parameters = parameters$hba(),
        setup_fn = setup_hba,
        get = "hba"
        )
      })

    shiny::observe({
      res$hbp <- dss_run_layer(
        layers = list(inp = res$inp, hba = res$hba),
        parameters = parameters$hbp(),
        setup_fn = setup_hbp,
        get = "hbp"
        )
      })

    shiny::observe({
      res$ca <- dss_run_layer(
        layers = list(inp = res$inp, hba = res$hba, hbp = res$hbp),
        parameters = parameters$ca(),
        setup_fn = setup_ca,
        get = "ca"
      )
    })

    shiny::observe({
      res$ct <- dss_run_layer(
        layers = list(inp = res$inp, hba = res$hba, hbp = res$hbp, ca = res$ca),
        parameters = parameters$ct(),
        setup_fn = setup_ct,
        get = "ct"
      )
    })

    return( res )

  })
}

dss_run_layer <- function(layers, parameters, setup_fn, get) {
  sim <- do.call(simulation_from_layers, layers)
  setup_fn_args <- c(list(simulation = sim), parameters)
  do.call(setup_fn, setup_fn_args) |>
    run_simulation(layer = get) |>
    get_layer(get)
}
