dss_server <- function(input, output, session) {
  clicked_cluster_id <- shiny::reactive({ input$map_shape_click$id })

  parameters <- dss_input_server("dss_input")
  layers <- dss_run_server("dss_output", parameters = parameters)
  dss_output_server("dss_output",
                    layers = layers,
                    clicked_cluster_id = clicked_cluster_id)

  output$map <- leaflet::renderLeaflet(plot_albufera_clusters()) |>
    shiny::snapshotExclude()

  shiny::observeEvent(input$hide_map_card, {
    shinyjs::hide("map_card")
  })

  shiny::observeEvent(input$show_map_card, {
    shinyjs::show("map_card")
  })

  shiny::observeEvent(input$show_about_modal, {
    shiny::showModal(dss_about_modal())
  })

  shiny::observeEvent(input$take_screenshot, {
    timestr <- Sys.time() |> format() |> gsub("[^0-9]", "", x = _)
    filename <- paste0("erahumed-screenshot-", timestr)
    shinyscreenshot::screenshot(filename = filename)
    })
}
