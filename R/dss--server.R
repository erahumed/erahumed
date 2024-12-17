dss_server <- function(input, output, session) {
  parameters <- dss_input_server("dss_input")
  layers <- dss_run_server("dss_output", parameters = parameters)
  dss_output_server("dss_output", layers = layers)

  output$map <- leaflet::renderLeaflet(plot_albufera_clusters()) |>
    shiny::snapshotExclude()

  shiny::observeEvent(input$hide_map_card, {
    shinyjs::hide("map_card")
  })

  shiny::observeEvent(input$show_map_card, {
    shinyjs::show("map_card")
  })

  shiny::observeEvent(input$take_screenshot, {
    shiny::showModal(shiny::modalDialog(
      "This feature has not been implemented yet.",
      title = shiny::p(shiny::icon("ban"), "Not implemented")
      ))
    })
}
