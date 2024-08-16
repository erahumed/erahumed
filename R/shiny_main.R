#' Launch the Shiny Application
#'
#' TODO #57
#'
#' @export
launch_app <- function() {
  if (!requireNamespace("sf")) {
    # {sf} is required to register S3 methods for {sf} objects
    stop("{sf} needs to be loaded for the correct functioning of the app.")
  }
  shiny::runApp(shiny_app(), launch.browser = TRUE)
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

shiny_app <- function() {
  shiny::shinyApp(ui = shiny_ui, server = shiny_server)
}

