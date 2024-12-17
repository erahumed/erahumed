inp_input_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::numericInput(ns("seed"),
                        "Seed for simulation",
                        value = 840,
                        step = 1),
    shiny::numericInput(ns("prop_jsendra"),
                        "Proportion of 'J.Sendra' variety",
                        value = 8,
                        min = 0,
                        max = 10,
                        step = 0.01),
    shiny::numericInput(ns("prop_bomba"),
                        "Proportion of 'Bomba' variety",
                        value = 1,
                        min = 0,
                        max = 10,
                        step = 0.01),
    shiny::numericInput(ns("prop_clearfield"),
                        "Proportion of 'Clearfield' variety",
                        value = 1,
                        min = 0,
                        max = 10,
                        step = 0.01),
    shiny::actionButton(ns("open_outflows_df_modal"), "Setup Outflows DF"),
    shiny::actionButton(ns("open_weather_df_modal"), "Setup Weather DF")

  )
}

inp_input_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    outflows_df <- csvInputServer("outflows", erahumed::albufera_outflows)
    shiny::observeEvent(input$open_outflows_df_modal, {
      shiny::showModal(shiny::modalDialog(
        csvInputUI(ns("outflows")),
        title = "Setup Outflows Dataset"
      ))
    })

    weather_df <- csvInputServer("weather", erahumed::albufera_weather)
    shiny::observeEvent(input$open_weather_df_modal, {
      shiny::showModal(shiny::modalDialog(
        csvInputUI(ns("weather")),
        title = "Setup Weather Dataset"
      ))
    })

    res <- shiny::reactive({
      list(
        variety_prop =
          c(input$prop_jsendra, input$prop_bomba, input$prop_clearfield),
        seed = input$seed,
        outflows_df = outflows_df(),
        weather_df = weather_df()
      )
    })

    return(res)
  })
}

inpUI <- function(id) {
  ns <- shiny::NS(id)

  outflows_tab_title <- "Hydrological data"
  weather_tab_title <- "Weather data"

  shiny::tabsetPanel(
    shiny::tabPanel(outflows_tab_title, csvInputUI(ns("outflows"))),
    shiny::tabPanel(weather_tab_title, csvInputUI(ns("weather"))),
    shiny::tabPanel("Setup",
                    shiny::numericInput(ns("seed"),
                                        "Seed for simulation",
                                        value = 840,
                                        step = 1),
                    shiny::numericInput(ns("prop_bomba"),
                                        "Proportion of 'Bomba' variety",
                                        value = 1,
                                        min = 0,
                                        max = 10,
                                        step = 0.01
                                        ),
                    shiny::numericInput(ns("prop_clearfield"),
                                        "Proportion of 'Clearfield' variety",
                                        value = 1,
                                        min = 0,
                                        max = 10,
                                        step = 0.01
                                        ),
                    shiny::numericInput(ns("prop_jsendra"),
                                        "Proportion of 'J.Sendra' variety",
                                        value = 8,
                                        min = 0,
                                        max = 10,
                                        step = 0.01
                    )

                    ),
    shiny::tabPanel("Filters",
                    shiny::dateRangeInput(inputId = ns("date_range"),
                                          label = "Date Range",
                                          start = as.Date("2020-01-01"),
                                          end = as.Date("2020-12-31")
                                          )
                    )
    )
}

inpServer <- function(id, layers, shared) {
  shiny::moduleServer(id, function(input, output, session) {
    weather_df <- csvInputServer("weather", erahumed::albufera_weather)

    outflows_df_raw <- csvInputServer("outflows", erahumed::albufera_outflows)
    outflows_df <- shiny::reactive({
      res <- outflows_df_raw()
      res <- res[res$date >= input$date_range[1], ]
      res <- res[res$date <= input$date_range[2], ]
      res
    })

    res <- shiny::reactive({
      simulation_from_layers() |>
        setup_inp(outflows_df = outflows_df(),
                  weather_df = weather_df(),
                  seed = input$seed,
                  variety_prop = c(input$prop_jsendra,
                                   input$prop_bomba,
                                   input$prop_clearfield)
                  ) |>
        run_simulation(layer = "inp") |>
        get_layer("inp")
    })

    return(res)
  })
}



