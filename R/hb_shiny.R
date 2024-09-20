hbUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tabsetPanel(
    shiny::tabPanel("Global Balance", hbGlobalUI(ns("lhb"))),
    shiny::tabPanel("Local Balance", hbLocalUI(ns("hb_local"))),
    shiny::tabPanel("Setup", hbSetupUI(ns("hb_setup")))
  )
}

hbServer <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    setup <- hbSetupServer("hb_setup")
    hbGlobalServer("lhb", setup, data)
    hbLocalServer("hb_local", setup, data)
  })
}



hbSetupUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(

    shiny::dateRangeInput(ns("date_range"),
                          "Select Date Range",
                          start = as.Date("2020-01-01"),
                          end = as.Date("2020-12-31")
                          ),
    shiny::numericInput(ns("ideal_flow_rate_cm"),
                        "Ideal Flow Rate [cm]",
                        value = 5,
                        min = 0,
                        max = 20,
                        step = 0.5
                        ),
    shiny::numericInput(ns("hbl_seed"),
                        "Seed for simulation",
                        value = 840,
                        step = 1
    ),

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
                           choices = lhb_var_labs(invert = TRUE),
                           selected = lhb_var_labs(invert = TRUE)[[1]]
                           ))
      ),

    plotly::plotlyOutput(ns("hb_plot"))
    )
}

hbGlobalServer <- function(id, setup, data) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    hb_data <- shiny::reactive({
      hb_data_full <- albufera_lhb(
        outflows_df = data()$outflows_df,
        petp_df = data()$petp_df
        )
      row_idx <- setup$date_range[1] <= hb_data_full$date
      row_idx <- row_idx & hb_data_full$date <= setup$date_range[2]
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
                                       choices = erahumed::albufera_clusters$cluster_id,
                                       selected = erahumed::albufera_clusters$cluster_id[1]
                                       )
                    )
      ),
    shiny::fluidRow(
      shiny::column(4, leaflet::leafletOutput(ns("albufera_map"))),
      shiny::column(8, plotly::plotlyOutput(ns("hb_plot")))
    )
  )
}

hbLocalServer <- function(id, setup, data) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$albufera_map <- leaflet::renderLeaflet(
      tryCatch(
        plot_albufera_clusters(seed = setup$hbl_seed),
        error = function(cnd) {
          cat("Error while loading Albufera Leaflet map.")
          return(NULL)
          },
        warning = function(cnd) {
          return(NULL)
          }
        )
      ) |>
      shiny::bindCache(setup$hbl_seed)

    shiny::observeEvent(input$albufera_map_shape_click, {
      click <- input$albufera_map_shape_click
      if (!is.null(click)) {
        shiny::updateSelectInput(session, "cluster_id", selected = click$id)
      }
    })

    hb_data <- shiny::reactive({
      withr::with_seed(setup$hbl_seed,
                       albufera_hb_local(
                         outflows_df = data()$outflows_df,
                         petp_df = data()$petp_df,
                         management_df = data()$management_df,
                         date_min = setup$date_range[1],
                         date_max = setup$date_range[2],
                         ideal_flow_rate_cm = setup$ideal_flow_rate_cm
                         )
                       )
      }) |>
      shiny::bindCache(digest::digest(data()), setup$hbl_seed, setup$date_range)

    output$hb_plot <- plotly::renderPlotly({
      shiny::req(hb_data())
      shiny::req(input$cluster_id)
      plot(hb_data(), cluster_id = input$cluster_id)
      }) |>
      shiny::bindCache(input$cluster_id,
                       digest::digest(data()),
                       setup$hbl_seed,
                       setup$date_range)
  })
}

