#' Launch the Shiny Application
#'
#' Documentation TBD.
#'
#' @export
launch_app <- function() {
  shiny::runApp(
    list(ui = shiny_ui, server = shiny_server),
    launch.browser = TRUE
  )
}

shiny_ui <- function() {
  shiny::fluidPage(
    shiny::titlePanel("ERAHUMED"),
    shiny::tabsetPanel(
      shiny::tabPanel("Hydrological Balance", hbUI("hb")),
      shiny::tabPanel("Second Tab", )
    )
  )
}

shiny_server <- function(input, output, session) {
  hbServer("hb")
}






