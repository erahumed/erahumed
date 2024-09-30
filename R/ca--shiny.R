caUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tabsetPanel(
    shiny::tabPanel("Output", NULL),
    shiny::tabPanel("Setup", NULL)
  )


}

caServer <- function(id, model) {
  shiny::moduleServer(id, function(input, output, session) {
    NULL
  })
}
