#' Launch the Shiny Application
#'
#' Documentation TBD.
#'
#' @export
launch_app <- function() {
  shiny::runApp(
    list(ui = shiny_ui(), server = shiny_server),
    launch.browser = TRUE
  )
}


shiny_ui <- function() {
  shiny::fluidPage(
    shiny::titlePanel("ERAHUMED"),
    shiny::tabsetPanel(
      shiny::tabPanel("Hydrological Balance",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            shiny::dateRangeInput("date_range",
                                  "Select Date Range",
                                  start = min(albufera_outflows$date),
                                  end = max(albufera_outflows$date),
                                  min = min(albufera_outflows$date),
                                  max = max(albufera_outflows$date)
                                  ),

            shiny::selectInput("variable", "Select Variable",
                               choices = var_labels(invert = TRUE),
                               selected = var_labels(invert = TRUE)[[1]]
                               ),

            ),
          shiny::mainPanel(
            plotly::plotlyOutput("hydro_plot")
          )
        )
      ),
      shiny::tabPanel("Second Tab", )
    )
  )
}

shiny_server <- function(input, output) {
  output$hydro_plot <- plotly::renderPlotly({
    df <- albufera_hydro_balance()
    df <- df[df$date >= input$date_range[1] & df$date <= input$date_range[2], ]


    vname <- input$variable
    vlab <- var_labels()[vname]
    vimp <- paste0(vname, "_is_imputed")

    imp <- df[[vimp]]

    df_obs <- df_imp <- df[, ]
    df_obs[[vname]][imp] <- NA
    df_imp[[vname]][!imp] <- NA

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
}



var_labels <- function(invert = F){
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

















## Refs
# https://plotly-r.com/linking-views-with-shiny.html#shiny-plotly-inputs

