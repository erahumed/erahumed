hbUI <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::dateRangeInput(ns("date_range"),
                              "Select Date Range",
                              start = min(albufera_outflows$date),
                              end = max(albufera_outflows$date),
                              min = min(albufera_outflows$date),
                              max = max(albufera_outflows$date)
        ),

        shiny::selectInput(ns("variable"), "Select Variable",
                           choices = hb_var_labels(invert = TRUE),
                           selected = hb_var_labels(invert = TRUE)[[1]]
        ),

      ),
      shiny::mainPanel(
        plotly::plotlyOutput(ns("hb_plot"))
      )
    )
  )
}

hbServer <- function(id)
{
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    hb_data_full <- hydro_balance()

    hb_data <- reactive({
      row_idx <-
        input$date_range[1] <= hb_data_full$date &
        hb_data_full$date <= input$date_range[2]
      hb_data_full[row_idx, ]
    })

    output$hb_plot <- plotly::renderPlotly({

      vname <- input$variable
      vlab <- hb_var_labels()[vname]
      vimp <- paste0(vname, "_is_imputed")

      imp <- hb_data()[[vimp]]

      df_obs <- df_imp <- hb_data()[, ]
      df_obs[[vname]][imp] <- NA
      df_imp[[vname]][!imp] <- NA

      ## Possibly helpful
      # https://plotly-r.com/linking-views-with-shiny.html#shiny-plotly-inputs
      plotly::plot_ly() |>
        plotly::add_trace(
          data = df_obs, x = ~date, y = ~get(vname),
          type = "scatter", mode = "lines",
          line = list(color = "blue", width = 2, dash = "solid"),
          name = "Observed Data"
        ) |>
        plotly::add_trace(
          data = df_imp, x = ~date, y = ~get(vname),
          type = "scatter", mode = "lines",
          line = list(color = "red", width = 2, dash = "dash"),
          name = "Imputed Data"
        ) |>
        plotly::layout(
          title = paste("Time Series of", vlab),
          xaxis = list(title = "Date"),
          yaxis = list(title = vlab)
        )
      })
    })
}

hb_var_labels <- function(invert = F){
  res <- c(
    level = "Lake Level [m]",
    volume = "Lake Volume [m\u{00B3}]",
    total_inflow = "Total Inflow [m\u{00B3} / s]",
    pujol = "Pujol Outflow [m\u{00B3} / s]",
    perellonet = "Perellonet Outflow [m\u{00B3} / s]",
    perello = "Perello Outflow [m\u{00B3} / s]",
    residence_time_days = "Residence Time [Days]"
  )

  if (!invert)
    return(res)

  res_inv <- names(res)
  names(res_inv) <- res

  return(res_inv)
}
