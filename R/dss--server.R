dss_server <- function(input, output, session) {
  output$map <- leaflet::renderLeaflet(plot_albufera_clusters())

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
