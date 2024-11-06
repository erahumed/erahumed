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
    shiny::tabPanel("INP: Input Data", inpUI("inp")),
    shiny::tabPanel("HBA: Hydrological Balance (Albufera)", hbaUI("hba")),
    shiny::tabPanel("HBP: Hydrological Balance (rice Paddies)", hbpUI("hbp")),
    shiny::tabPanel("CA: Chemical Applications", caUI("ca")),
    shiny::tabPanel("CT: Chemical Transport", ctUI("ct")),
    )
}

shiny_server <- function(input, output, session) {
  model <- shiny::reactiveVal(erahumed_simulation())
  inp_res <- inpServer("inp", model)
  hba_res <- hbaServer("hba", inp_res)
  hbp_res <- hbpServer("hbp", hba_res)
  ca_res <- caServer("ca", hbp_res)
  ct_res <- ctServer("ct", ca_res)

}



