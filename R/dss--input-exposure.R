dss_input_exposure_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    ca_input_ui(ns("ca")),
    ctc_input_ui(ns("ctc"))
  )

}

dss_input_exposure_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    param_ca <- ca_input_server("ca")
    param_ctc <- ctc_input_server("ctc")

    shiny::reactive( c(param_ca(), param_ctc()) )
  })
}
