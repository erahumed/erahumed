#' ERAHUMED Decision Support System dashboard
#'
#' Launches the ERAHUMED Decision Support System dashboard Shiny app.
#'
#' @export
launch_dss <- function()
  shiny::runApp(dss(), launch.browser = TRUE)

dss <- function() {
  shiny::shinyApp(ui = dss_ui, server = dss_server)
}
