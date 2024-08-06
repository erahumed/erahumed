inputsUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tabsetPanel(
    shiny::tabPanel("Lake Levels and Outflows", ),
    shiny::tabPanel("Precipitation and Evapotranspiration", ),
    shiny::tabPanel("Paddy Management", )
  )
}

inputsServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}
