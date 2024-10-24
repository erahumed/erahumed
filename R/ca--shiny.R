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
        )
      ),

      shiny::fluidRow(
        shiny::column(4, leaflet::leafletOutput(ns("albufera_map"))),
        shiny::column(8, plotly::plotlyOutput(ns("plot")))
        )
      ),


    shiny::tabPanel("Application Schedules", csvInputUI(ns("ca_schedules"))),

    shiny::tabPanel("Setup",
                    shiny::numericInput(ns("seed"),
                                        "Seed for simulation",
                                        value = 840,
                                        step = 1),
                    )


  )


}

caServer <- function(id, model) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ca_schedules_df <- csvInputServer("ca_schedules",
                                    erahumed::albufera_ca_schedules)

    res <- shiny::reactive({
      compute_ca(model = model(), ca_schedules_df = ca_schedules_df())
      })

    output$albufera_map <- leaflet::renderLeaflet(
      tryCatch(
        plot_albufera_clusters(seed = input$seed),
        error = function(cnd) {
          cat("Error while loading Albufera Leaflet map.")
          return(NULL)
        },
        warning = function(cnd) {
          return(NULL)
        }
      )
    )

    shiny::observeEvent(input$albufera_map_shape_click, {
      click <- input$albufera_map_shape_click
      if (!is.null(click)) {
        shiny::updateSelectInput(session, "cluster_id", selected = click$id)
      }
    })

    output$plot <- plotly::renderPlotly({
      shiny::req(input$cluster_id)
      plot(ca(res()), cluster_id = input$cluster_id)
    })

    return(res)
  })
}
