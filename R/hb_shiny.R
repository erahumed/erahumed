hbUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tabsetPanel(
    shiny::tabPanel("Global Balance", hbGlobalUI(ns("hb_global"))),
    shiny::tabPanel("Local Balance", hbLocalUI(ns("hb_local")))
  )
}

hbServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    hbGlobalServer("hb_global")
    hbLocalServer("hb_local")
  })
}

hbGlobalUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(

    shiny::fluidRow(
      shiny::column(4,
        shiny::dateRangeInput(ns("date_range"),
                              "Select Date Range",
                              start = min(albufera_outflows$date),
                              end = max(albufera_outflows$date),
                              min = min(albufera_outflows$date),
                              max = max(albufera_outflows$date)
                              )),
      shiny::column(4,
        shiny::selectInput(ns("variable"), "Select Variable",
                           choices = hb_global_var_labs(invert = TRUE),
                           selected = hb_global_var_labs(invert = TRUE)[[1]]
                           ))
      ),

    plotly::plotlyOutput(ns("hb_plot"))
    )
}

hbGlobalServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    hb_data_full <- albufera_hb_global()

    hb_data <- shiny::reactive({
      row_idx <-
        input$date_range[1] <= hb_data_full$date &
        hb_data_full$date <= input$date_range[2]
      hb_data_full[row_idx, ]
    })

    output$hb_plot <- plotly::renderPlotly( plot(hb_data(), input$variable) )

    })
}

hbLocalUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(4,
                    shiny::dateRangeInput(ns("date_range"),
                                          "Select Date Range",
                                          start = "2010-01-01",
                                          end = "2011-12-31",
                                          min = min(albufera_outflows$date),
                                          max = max(albufera_outflows$date)
                    )),
      shiny::column(4,
                    shiny::selectInput(ns("cluster_id"),
                                       "Select Cluster",
                                       choices = albufera_clusters$cluster_id,
                                       selected = albufera_clusters$cluster_id[1]
                                       )
                    ),
      shiny::column(4, shiny::actionButton(ns("run_button"), "Run"))
      ),
    shiny::fluidRow(
      shiny::column(4, leaflet::leafletOutput(ns("map"))),
      shiny::column(8, plotly::plotlyOutput(ns("plot")))
    )
  )
}

hbLocalServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    data <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$run_button, {
      data(albufera_hb_local(date_min = input$date_range[1],
                             date_max = input$date_range[2])
           )
    })

    output$plot <- plotly::renderPlotly({
      shiny::req(data())
      shiny::req(input$cluster_id)

      plot(data(), cluster_id = input$cluster_id)
    })

    output$map <- leaflet::renderLeaflet({
      plot_albufera_clusters()
    })

    shiny::observeEvent(input$map_shape_click, {
      click <- input$map_shape_click
      if (!is.null(click)) {
        shiny::updateSelectInput(session, "cluster_id", selected = click$id)
      }
    })

  })
}

