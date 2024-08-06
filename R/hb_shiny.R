hbUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tabsetPanel(
    shiny::tabPanel("Global Balance", hbGlobalUI(ns("hb_global"))),
    shiny::tabPanel("Local Balance", hbLocalUI(ns("hb_local"))),
    shiny::tabPanel("Setup", hbSetupUI(ns("hb_setup")))
  )
}

hbServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    setup <- hbSetupServer("hb_setup")
    hbGlobalServer("hb_global", setup)
    hbLocalServer("hb_local", setup)
  })
}



hbSetupUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(

    shiny::dateRangeInput(ns("date_range"),
                          "Select Date Range",
                          start = as.Date("2020-01-01"),
                          end = as.Date("2020-12-31"),
                          min = min(albufera_outflows$date),
                          max = max(albufera_outflows$date)
                          ),
    shiny::numericInput(ns("ideal_flow_rate_cm"),
                        "Ideal Flow Rate [cm]",
                        value = 5,
                        min = 0,
                        max = 20,
                        step = 0.5
                        )

  )
}

hbSetupServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    return(input)
  })
}



hbGlobalUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(

    shiny::fluidRow(
      shiny::column(4,
        shiny::selectInput(ns("variable"), "Select Variable",
                           choices = hb_global_var_labs(invert = TRUE),
                           selected = hb_global_var_labs(invert = TRUE)[[1]]
                           ))
      ),

    plotly::plotlyOutput(ns("hb_plot"))
    )
}

hbGlobalServer <- function(id, setup) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    hb_data_full <- albufera_hb_global()

    hb_data <- shiny::reactive({
      row_idx <-
        setup$date_range[1] <= hb_data_full$date & hb_data_full$date <= setup$date_range[2]
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
                    shiny::selectInput(ns("cluster_id"),
                                       "Select Cluster",
                                       choices = albufera_clusters$cluster_id,
                                       selected = albufera_clusters$cluster_id[1]
                                       )
                    ),
      shiny::column(4, shiny::actionButton(ns("run_button"), "Run"))
      ),
    shiny::fluidRow(
      shiny::column(4, leaflet::leafletOutput(ns("albufera_map"))),
      shiny::column(8, plotly::plotlyOutput(ns("hb_plot")))
    )
  )
}

hbLocalServer <- function(id, setup) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$albufera_map <- leaflet::renderLeaflet({ plot_albufera_clusters() })

    shiny::observeEvent(input$albufera_map_shape_click, {
      click <- input$albufera_map_shape_click
      if (!is.null(click)) {
        shiny::updateSelectInput(session, "cluster_id", selected = click$id)
      }
    })

    hb_data <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$run_button, {
      hb_data(
        albufera_hb_local(date_min = setup$date_range[1],
                          date_max = setup$date_range[2],
                          ideal_flow_rate_cm = setup$ideal_flow_rate_cm)
        )
      })

    output$hb_plot <- plotly::renderPlotly({
      shiny::req(hb_data())
      shiny::req(input$cluster_id)

      plot(hb_data(), cluster_id = input$cluster_id)
      })

  })
}

