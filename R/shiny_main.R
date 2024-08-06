#' Launch the Shiny Application
#'
#' TODO: Documentation TBD.
#'
#' @export
launch_app <- function() {
  if (!requireNamespace("sf")) {
    # {sf} is required to register S3 methods for {sf} objects
    stop("{sf} needs to be loaded for the correct functioning of the app.")
  }
  shiny::runApp(shiny_app(), launch.browser = TRUE)
}


# TODO: Currently, there is a tabsetPanel nested within the hbUI, and each tab
# represents a submodule called by the hb module. Another option would be to
# use dropdown in the navbarPage, and call directly each submodule from the
# dropdown in the main app. To consider later.

shiny_ui <- function() {
  shiny::navbarPage(
    "ERAHUMED",
    shiny::tabPanel("Hydrological Balance", hbUI("hb")),
    shiny::tabPanel("Inputs", inputsUI("inputs"))
    )
}

shiny_server <- function(input, output, session) {
  hbServer("hb")
  inputsServer("inputs")
}

shiny_app <- function() {
  shiny::shinyApp(ui = shiny_ui, server = shiny_server)
}

