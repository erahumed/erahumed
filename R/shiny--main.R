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
  app_title <- paste0("ERAHUMED v", utils::packageVersion("erahumed"))
  favicon_path <- system.file("app/www/favicon.ico", package = "erahumed")
  favicon <- base64enc::dataURI(file = favicon_path, mime = "image/x-icon")



  shiny::navbarPage(
    title = app_title,
    windowTitle = app_title,
    header = shiny::tags$head(
      shiny::tags$link(rel = "icon", type = "image/x-icon", href = favicon)
      ),
    shiny::tabPanel("INP: Input Data", inpUI("inp")),
    shiny::tabPanel("HBA: Hydrological Balance (Albufera)", hbaUI("hba")),
    shiny::tabPanel("HBP: Hydrological Balance (rice Paddies)", hbpUI("hbp")),
    shiny::tabPanel("CA: Chemical Applications", caUI("ca")),
    shiny::tabPanel("CT: Chemical Transport", ctUI("ct")),
    )
}

shiny_server <- function(input, output, session) {
  shared <- shiny::reactiveValues(
    map = plot_albufera_clusters(),
    selected_cluster_id = albufera_clusters$cluster_id[1]
  )
  simulation <- shiny::reactiveVal(erahumed_simulation())

  inp_res <- inpServer("inp", simulation, shared)
  hba_res <- hbaServer("hba", inp_res, shared)
  hbp_res <- hbpServer("hbp", hba_res, shared)
  ca_res <- caServer("ca", hbp_res, shared)
  ct_res <- ctServer("ct", ca_res, shared)

}
