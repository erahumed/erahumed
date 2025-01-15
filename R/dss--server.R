dss_server <- function(input, output, session) {
  clicked_cluster_id <- shiny::reactive({ input$map_shape_click$id })

  parameters <- dss_input_server("dss_input")
  param_hash <- shiny::reactive( digest::digest(lapply(parameters, \(r) r())) )
  shiny::observe(shiny::updateActionButton(session, "run", disabled = FALSE)) |>
    shiny::bindEvent(param_hash())

  layers <- dss_run_server("dss_run", parameters = parameters, run = input$run)
  layers_hash <- shiny::reactive({
    shiny::reactiveValuesToList(layers) |>
      lapply(identity) |>
      digest::digest()
    })
  shiny::observe(shiny::updateActionButton(session, "run", disabled = TRUE)) |>
    shiny::bindEvent(layers_hash())

  dss_output_server("dss_output",
                    layers = layers,
                    clicked_cluster_id = clicked_cluster_id)

  vmap <- shiny::reactive( get_layer_aux(layers$inp)[["cluster_variety_map"]] )

  output$map <- leaflet::renderLeaflet(
    plot_albufera_clusters(cluster_variety_map = vmap())
    ) |>
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

  sever::sever(
    opacity = 0.8,
    bg_color = "black",
    html = sever::sever_default(
      title = "Session Ended",
      subtitle = paste0("You have been disconnected from the server. ",
                        "Please check your internet connection and click ",
                        "the button below to attempt to reconnect. If the ",
                        "issue persists, please contact the administrators."),
      button = "Reconnect",
      button_class = "info"
  ))
}
