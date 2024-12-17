dss_output_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::p("Output content.")
}

dss_output_server <- function(id, layers) {
  shiny::moduleServer(id, function(input, output, session) {

  })
}
