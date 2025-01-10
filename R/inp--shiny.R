inp_input_ui <- function(id) {
  ns <- shiny::NS(id)

  tltp <- function(param) param_tooltip(layer = "inp", param = param)

  date_range_tooltip <- bslib::tooltip(
    shiny_icon("question-circle"),
    "Date range constraining subsequent calculations and analyses.",
    placement = "right"
    )

  outflows_df_desc <- erahumed_param_desc("outflows_df", "inp", strip_roxy = T)
  weather_df_desc <- erahumed_param_desc("weather_df", "inp", strip_roxy = T)

  shiny::tagList(
    shiny::numericInput(ns("seed"),
                        shiny::p("Seed for simulation", tltp("seed")),
                        value = 840,
                        step = 1),

    shiny::dateRangeInput(inputId = ns("date_range"),
                          label = shiny::p("Date Range", date_range_tooltip),

                          start = as.Date("2020-01-01"),
                          end = as.Date("2020-12-31")),

    shiny::div(
      shiny::p(shiny::strong("Rice variety proportions"), tltp("variety_prop")),
      shiny::numericInput(ns("prop_jsendra"),
                          "Proportion of 'J.Sendra'",
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
      style = "border: 1px solid lightgray; padding: 10px; margin: 5px;",
      ),



    shiny::actionButton(ns("open_outflows_df_modal"), "Setup Outflows DF") |>
      bslib::tooltip(outflows_df_desc),
    shiny::actionButton(ns("open_weather_df_modal"), "Setup Weather DF") |>
      bslib::tooltip(weather_df_desc)
  )
}

inp_input_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    tltp <- function(param) param_tooltip(layer = "inp", param = param)

    outflows_df_raw <- csvInputServer("outflows", erahumed::albufera_outflows)
    outflows_df <- shiny::reactive({
      res <- outflows_df_raw()
      res <- res[res$date >= input$date_range[1], ]
      res <- res[res$date <= input$date_range[2], ]
      res
      })
    shiny::observeEvent(input$open_outflows_df_modal, {
      shiny::showModal(shiny::modalDialog(
        csvInputUI(
          ns("outflows"),
          columns = erahumed_docs("layers", "inp", "parameters", "outflows_df", "columns")
          ),
        title = shiny::p("Setup Outflows Dataset", tltp("outflows_df")),
        size = "xl"
      ))
    })

    weather_df <- csvInputServer("weather", erahumed::albufera_weather)
    shiny::observeEvent(input$open_weather_df_modal, {
      shiny::showModal(shiny::modalDialog(
        csvInputUI(
          ns("weather"),
          columns = erahumed_docs("layers", "inp", "parameters", "weather_df", "columns")
          ),
        title = shiny::p("Setup Weather Dataset", tltp("weather_df")),
        size = "xl"
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
