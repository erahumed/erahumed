ca_input_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::actionButton(ns("open_ca_schedules_df_modal"), "Setup Applications DF")

  )
}

ca_input_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ca_schedules_df <- csvInputServer("applications", erahumed::albufera_ca_schedules)
    shiny::observeEvent(input$open_ca_schedules_df_modal, {
      shiny::showModal(shiny::modalDialog(
        csvInputUI(ns("applications")),
        title = "Setup Applications Dataset",
        size = "xl"
      ))
    })

    shiny::reactive({
      list(ca_schedules_df = ca_schedules_df())
    })
  })
}

caUI <- function(id) {
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


    shiny::tabPanel("Application Schedules", csvInputUI(ns("ca_schedules"))),

    shiny::tabPanel("Setup", )


  )


}

caServer <- function(id, inp, hba, hbp, shared) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ca_schedules_df <-
      csvInputServer("ca_schedules", erahumed::albufera_ca_schedules)

    res <- shiny::reactive({
      simulation_from_layers(inp = inp(), hba = hba(), hbp = hbp()) |>
        setup_ca(ca_schedules_df = ca_schedules_df()) |>
        run_simulation(layer = "ca") |>
        get_layer("ca")
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
      plot(res(), cluster_id = input$cluster_id)
    })

    output$downloadData <- shiny::downloadHandler(
      filename = function() paste0("output-ca-", Sys.Date(), ".csv"),
      content = \(file) readr::write_csv(get_layer_output(res()), file)
    )

    return(res)
  })
}
