#' ERAHUMED Decision Support System dashboard
#'
#' Launches the ERAHUMED Decision Support System dashboard Shiny app.
#'
#' @export
launch_dss <- function() launch_dss_v0()

dss <- function() {
  shiny::shinyApp(ui = dss_ui, server = dss_server)
}

dss_ui <- function() {
  dss_ui_v0()
}

dss_server <- function(input, output, session) {
  dss_server_v0(input, output, session)
}

