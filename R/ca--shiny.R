caUI <- function(id) {
  ns <- shiny::NS(id)

  clusters <- erahumed::albufera_clusters$cluster_id

  shiny::tabsetPanel(
    shiny::tabPanel(
      "Output",

      shiny::fluidRow(
        shiny::column(4,
                      shiny::selectInput(ns("cluster_id"),
                                         "Select Cluster",
                                         choices = clusters,
                                         selected = clusters[[1]]
                      )
        ),
        shiny::column(4,
                      shiny::downloadButton(ns("downloadData"), "Download Data")
        )
      ),

      shiny::fluidRow(
        shiny::column(4, leaflet::leafletOutput(ns("map")) |>
                        shinycssloaders::withSpinner()
                      ),
        shiny::column(8, plotly::plotlyOutput(ns("plot")) |>
                        shinycssloaders::withSpinner()
                      )
        )
      ),


    shiny::tabPanel("Application Schedules", csvInputUI(ns("ca_schedules"))),

    shiny::tabPanel("Setup", )


  )


}

caServer <- function(id, simulation, shared) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ca_schedules_df <-
      csvInputServer("ca_schedules", erahumed::albufera_ca_schedules)

    res <- shiny::reactive({
      simulation() |>
        setup_ca(ca_schedules_df = ca_schedules_df()) |>
        run_simulation(layer = "ca")
      })

    output$map <- leaflet::renderLeaflet(shared$map)

    shiny::observeEvent(input$map_shape_click, {
      click <- input$map_shape_click
      if (!is.null(click))
        shiny::updateSelectInput(session, "cluster_id", selected = click$id)
    })

    shiny::observeEvent(input$cluster_id, {
      if (input$cluster_id != shared$selected_cluster_id)
        shared$selected_cluster_id <- input$cluster_id
    })

    shiny::observeEvent(shared$selected_cluster_id, {
      if (input$cluster_id != shared$selected_cluster_id)
        shiny::updateSelectInput(session, "cluster_id",
                                 selected = shared$selected_cluster_id)
    })

    output$plot <- plotly::renderPlotly({
      shiny::req(input$cluster_id)

      res() |>
        get_layer("ca") |>
        plot(cluster_id = input$cluster_id)
    })

    output$downloadData <- shiny::downloadHandler(
      filename = function() paste0("output-ca-", Sys.Date(), ".csv"),
      content = \(file) readr::write_csv(get_layer_output(res(), "ca"), file)
    )

    return(res)
  })
}
