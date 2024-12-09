hbpUI <- function(id) {
  ns <- shiny::NS(id)

  clusters <- albufera_clusters$cluster_id

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

      dygraphs::dygraphOutput(ns("plot")) |> shinycssloaders::withSpinner()

      ),


    shiny::tabPanel("Management Schedule", csvInputUI(ns("management"))),

    shiny::tabPanel("Setup",
                    shiny::numericInput(ns("ideal_flow_rate_cm"),
                                        "Ideal Flow Rate [cm]",
                                        value = 5,
                                        min = 0,
                                        max = 20,
                                        step = 0.5
                                        )
                    )
  )


}

hbpServer <- function(id, inp, hba, shared) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    management_df <- csvInputServer("management", erahumed::albufera_management)

    res <- shiny::reactive({
       simulation_from_layers(inp = inp(), hba = hba()) |>
        setup_hbp(management_df = management_df(),
                  ideal_flow_rate_cm = input$ideal_flow_rate_cm) |>
        run_simulation(layer = "hbp") |>
        get_layer("hbp")
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


    output$plot <- dygraphs::renderDygraph({
      shiny::req(input$cluster_id)
      plot(res(), cluster_id = shared$selected_cluster_id)
    })

    output$downloadData <- shiny::downloadHandler(
      filename = function() paste0("output-hbp-", Sys.Date(), ".csv"),
      content = \(file) readr::write_csv(get_layer_output(res()), file)
      )

    return(res)

  })
}
