dss_input_risk_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    "TBD"
  )

}

dss_input_risk_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
  shiny::reactive( list() )
  })
}
