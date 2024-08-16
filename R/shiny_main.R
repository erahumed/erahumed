#' Launch the Shiny Application
#'
#' TODO #57
#'
#' @export
launch_app <- function() {
  shiny::runApp(shiny_app(), launch.browser = TRUE)
}

shiny_app <- function() {
  shiny::shinyApp(ui = shiny_ui, server = shiny_server)
}

shiny_ui <- function() {
  shiny::navbarPage(
    "ERAHUMED",
    shiny::tabPanel("Hydrological Balance", hbUI("hb")),
    shiny::tabPanel("Data", dataUI("data"))
    )
}

shiny_server <- function(input, output, session) {
  data <- dataServer("data")
  hbServer("hb", data)
}



