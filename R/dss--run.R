dss_run_server <- function(id, parameters, run) {
  shiny::moduleServer(id, function(input, output, session) {

    layers <- shiny::reactiveValues()

    shiny::observe({
      layers$inp <- dss_run_layer(
        layers = list(),
        parameters = parameters$inp(),
        setup_fn = setup_inp,
        get = "inp"
      )

      layers$hbl <- dss_run_layer(
        layers = list(inp = layers$inp),
        parameters = parameters$hbl(),
        setup_fn = setup_hbl,
        get = "hbl"
      )

      layers$hbc <- dss_run_layer(
        layers = list(inp = layers$inp, hbl = layers$hbl),
        parameters = parameters$hbc(),
        setup_fn = setup_hbc,
        get = "hbc"
      )

      layers$hbd <- dss_run_layer(
        layers = list(inp = layers$inp, hbl = layers$hbl, hbc = layers$hbc),
        parameters = parameters$hbd(),
        setup_fn = setup_hbd,
        get = "hbd"
      )

      layers$ca <- dss_run_layer(
        layers = list(inp = layers$inp, hbl = layers$hbl, hbc = layers$hbc, hbd = layers$hbd),
        parameters = parameters$ca(),
        setup_fn = setup_ca,
        get = "ca"
      )

      layers$ct <- dss_run_layer(
        layers = list(inp = layers$inp, hbl = layers$hbl, hbc = layers$hbc, hbd = layers$hbd, ca = layers$ca),
        parameters = parameters$ct(),
        setup_fn = setup_ct,
        get = "ct"
      )
    }) |>
      shiny::bindEvent(run, ignoreNULL = FALSE)

    return( layers )

  })
}

dss_run_layer <- function(layers, parameters, setup_fn, get) {
  sim <- do.call(simulation_from_layers, layers)
  setup_fn_args <- c(list(simulation = sim), parameters)
  do.call(setup_fn, setup_fn_args) |>
    run_simulation(layer = get) |>
    get_layer(get)
}
