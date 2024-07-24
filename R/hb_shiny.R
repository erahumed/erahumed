hbUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tabsetPanel(
    shiny::tabPanel("Global Balance", hbGlobalUI(ns("hb_global"))),
    shiny::tabPanel("Local Balance", hbLocalUI(ns("hb_local")))
  )
}

hbServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    hbGlobalServer("hb_global")
    hbLocalServer("hb_local")
  })
}

hbGlobalUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(

    shiny::fluidRow(
      shiny::column(4,
        shiny::dateRangeInput(ns("date_range"),
                              "Select Date Range",
                              start = min(albufera_outflows$date),
                              end = max(albufera_outflows$date),
                              min = min(albufera_outflows$date),
                              max = max(albufera_outflows$date)
                              )),
      shiny::column(4,
        shiny::selectInput(ns("variable"), "Select Variable",
                           choices = hb_var_labels(invert = TRUE),
                           selected = hb_var_labels(invert = TRUE)[[1]]
                           ))
      ),

    plotly::plotlyOutput(ns("hb_plot"))
    )
}

hbGlobalServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    hb_data_full <- albufera_hydro_balance_global()

    hb_data <- shiny::reactive({
      row_idx <-
        input$date_range[1] <= hb_data_full$date &
        hb_data_full$date <= input$date_range[2]
      hb_data_full[row_idx, ]
    })

    output$hb_plot <- plotly::renderPlotly( plot(hb_data(), input$variable) )

    })
}

hbLocalUI <- function(id) {
  ns <- shiny::NS(id)

  actionButton(ns("run_button"), "Run Calculation")

  }

hbLocalServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    result <- reactiveVal(NULL)

    shiny::observeEvent(input$run_button, {
      cat("Pressed button!")
      #output$df <- albufera_hydro_balance_local()
    })

  })
}

hb_var_labels <- function(invert = F) {
  res <- c(
    level = "Lake Level [m]",
    volume = "Lake Volume [m\u{00B3}]",
    outflow_total = "Total Outflow [m\u{00B3} / s]",
    pujol = "Pujol Outflow [m\u{00B3} / s]",
    perellonet = "Perellonet Outflow [m\u{00B3} / s]",
    perello = "Perello Outflow [m\u{00B3} / s]",
    inflow_total = "Total Inflow [m\u{00B3} / s]",
    residence_time_days = "Residence Time [Days]"
  )

  if (!invert)
    return(res)

  res_inv <- names(res)
  names(res_inv) <- res

  return(res_inv)
}
