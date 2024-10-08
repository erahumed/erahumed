hbpUI <- function(id) {
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


    shiny::tabPanel("Management Schedule", csvInputUI(ns("management"))),

    shiny::tabPanel("Setup",
                    shiny::numericInput(ns("ideal_flow_rate_cm"),
                                        "Ideal Flow Rate [cm]",
                                        value = 5,
                                        min = 0,
                                        max = 20,
                                        step = 0.5
                                        ),
                    shiny::numericInput(ns("seed"),
                                        "Seed for simulation",
                                        value = 840,
                                        step = 1
                                        ),
                    )
  )


}

hbpServer <- function(id, model) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    management_df <- csvInputServer("management", erahumed::albufera_management)

    res <- shiny::reactive({

      withr::with_seed(input$seed, {
        compute_hbp(model = model(),
                    management_df = management_df(),
                    ideal_flow_rate_cm = input$ideal_flow_rate_cm
                    )
        })

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
      plot(hbp(res()), cluster_id = input$cluster_id)
    })

    return(res)
  })
}
