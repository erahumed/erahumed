#' ERAHUMED Decision Support System dashboard
#'
#' Launches the ERAHUMED Decision Support System dashboard Shiny app.
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
    shiny::tabPanel("Input Data", inpUI("inp")),
    shiny::tabPanel("Hydrological Balance (Albufera)", hbaUI("hba")),
    shiny::tabPanel("Hydrological Balance (Rice Paddies)", hbpUI("hbp")),
    shiny::tabPanel("Chemicals Applications", caUI("ca"))
    )
}

shiny_server <- function(input, output, session) {
  model <- shiny::reactiveVal(erahumed_model())
  inp_res <- inpServer("inp", model)
  hba_res <- hbaServer("hba", inp_res)
  hbp_res <- hbpServer("hbp", hba_res)
  ca_res <- caServer("ca", hbp_res)
}



