#' ERAHUMED Decision Support System Graphical User Interface
#'
#' Opens the graphical user interface (GUI) of the ERAHUMED Decision Support
#' System. This is a Shiny application that provides access to the package's
#' functionalities through a point-and-click interface, and can be used as an
#' alternative to calling the functions programmatically.
#'
#' @export
launch_dss <- function()
  shiny::runApp(dss(), launch.browser = TRUE)

dss <- function() {
  shiny::shinyApp(ui = dss_ui, server = dss_server)
}
