inpUI <- function(id) {
  ns <- shiny::NS(id)

  outflows_tab_title <- "Hydrological data"
  weather_tab_title <- "Weather data"

  shiny::tabsetPanel(
    shiny::tabPanel(outflows_tab_title, csvInputUI(ns("outflows"))),
    shiny::tabPanel(weather_tab_title, csvInputUI(ns("weather"))),
    shiny::tabPanel("Filters",
                    shiny::dateRangeInput(inputId = ns("date_range"),
                                          label = "Date Range",
                                          start = as.Date("2020-01-01"),
                                          end = as.Date("2020-12-31")
                                          )
                    )
    )
}

inpServer <- function(id, simulation, shared) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    weather_df <- csvInputServer("weather", erahumed::albufera_weather)

    outflows_df_raw <- csvInputServer("outflows", erahumed::albufera_outflows)
    outflows_df <- shiny::reactive({
      res <- outflows_df_raw()
      res <- res[res$date >= input$date_range[1], ]
      res <- res[res$date <= input$date_range[2], ]
      res
    })

    res <- shiny::reactive({
      simulation() |>
        setup_inp(outflows_df = outflows_df(), weather_df = weather_df()) |>
        run_simulation(layer = "inp")
    })

    return(res)
  })
}



