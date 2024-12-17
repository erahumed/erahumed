dss_run_server <- function(id, parameters) {
  shiny::moduleServer(id, function(input, output, session) {

    inp <- shiny::reactive({
      dss_run_layer(
        layers = list(),
        parameters = parameters$inp(),
        setup_fn = setup_inp,
        get = "inp"
        )
      })

    hba <- shiny::reactive({
      dss_run_layer(
        layers = list(inp = inp()),
        parameters = parameters$hba(),
        setup_fn = setup_hba,
        get = "hba"
        )
      })

    hbp <- shiny::reactive({
      dss_run_layer(
        layers = list(inp = inp(), hba = hba()),
        parameters = parameters$hbp(),
        setup_fn = setup_hbp,
        get = "hbp"
        )
      })

    ca <- shiny::reactive({
      dss_run_layer(
        layers = list(inp = inp(), hba = hba(), hbp = hbp()),
        parameters = parameters$ca(),
        setup_fn = setup_ca,
        get = "ca"
      )
    })

    ct <- shiny::reactive({
      dss_run_layer(
        layers = list(inp = inp(), hba = hba(), hbp = hbp(), ca = ca()),
        parameters = parameters$ct(),
        setup_fn = setup_ct,
        get = "ct"
      )
    })

    return( list(inp = inp, hba = hba, hbp = hbp, ca = ca, ct = ct) )

  })
}

dss_run_layer <- function(layers, parameters, setup_fn, get) {
  sim <- do.call(simulation_from_layers, layers)
  setup_fn_args <- c(list(simulation = sim), parameters)
  do.call(setup_fn, setup_fn_args) |>
    run_simulation(layer = get) |>
    get_layer(get)
}
