inpUI <- function(id) {
  ns <- shiny::NS(id)

  outflows_tab_title <- "Lake Levels and Outflows"
  petp_tab_title <- "Precipitation and Evapotranspiration"

  shiny::tabsetPanel(
    shiny::tabPanel(outflows_tab_title, csvInputUI(ns("outflows"))),
    shiny::tabPanel(petp_tab_title, csvInputUI(ns("petp"))),
    shiny::tabPanel("Filters",
                    shiny::dateRangeInput(inputId = ns("date_range"),
                                          label = "Date Range",
                                          start = as.Date("2020-01-01"),
                                          end = as.Date("2020-12-31")
                                          )
                    )
    )
}

inpServer <- function(id, model) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    weather_df <- csvInputServer("petp", erahumed::albufera_weather)

    outflows_df_raw <- csvInputServer("outflows", erahumed::albufera_outflows)
    outflows_df <- shiny::reactive({
      res <- outflows_df_raw()
      res <- res[res$date >= input$date_range[1], ]
      res <- res[res$date <= input$date_range[2], ]
      res
    })

    res <- shiny::reactive({
      compute_inp(model(), outflows_df = outflows_df(), weather_df = weather_df())
    })
    return(res)
  })
}



