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
    shiny::tabPanel("Hydrological Balance (Rice Paddies)", hbaUI("hbp")),
    shiny::tabPanel("Chemicals Applications", caUI("ca"))
    )
}

shiny_server <- function(input, output, session) {
  inp_res <- inpServer("inp")
  hba_res <- hbaServer("hba", inp_res = inp_res)
  hbp_res <- hbpServer("hbp", hba_res = hba_res)
  ca_res <- caServer("ca", hbp_res = hbp_res)
}



