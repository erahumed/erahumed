#' Launch the Shiny Application
#'
#' Documentation TBD.
#'
#' @export
launch_app <- function() {
  shiny::shinyApp(ui = shiny_ui(), server = shiny_server)
}


shiny_ui <- function() {
  shiny::fluidPage(
    shiny::titlePanel("Albufera Hydrological Balance"),
    shiny::tabsetPanel(
      shiny::tabPanel("Hydrological Balance",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            shiny::dateRangeInput("dateRange",
                                  "Select Date Range",
                                  start = min(albufera_outflows$date),
                                  end = max(albufera_outflows$date),
                                  min = min(albufera_outflows$date),
                                  max = max(albufera_outflows$date)
                                  ),

            selectInput("variable", "Select Variable",
                        choices = var_labels(invert = TRUE),
                        selected = var_labels(invert = TRUE)[[1]]
                        ),
            ),
          shiny::mainPanel(
            shiny::plotOutput("levelPlot")
          )
        )
      ),
      shiny::tabPanel("Second Tab", )
    )
  )
}

shiny_server <- function(input, output) {
  output$levelPlot <- shiny::renderPlot({
    df <- albufera_outflows
    df <- df[df$date >= input$dateRange[1] & df$date <= input$dateRange[2], ]
    vl <- var_labels()[input$variable]

    plot(df$date, df[[input$variable]],
         type = "l", col = "blue",
         xlab = "Date", ylab = vl,
         main = paste("Time Series of", vl))
  })
}



var_labels <- function(invert = F){
  res <- c(
    level = "Lake Level [m]",
    pujol = "Pujol Outflow [m^3 / s]",
    perellonet = "Perellonet Outflow [m^3 / s]",
    perello = "Perello Outflow [m^3 / s]"
  )

  if (!invert)
    return(res)

  res_inv <- names(res)
  names(res_inv) <- res

  return(res_inv)
}

