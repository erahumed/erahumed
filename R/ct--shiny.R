ctUI <- function(id) {
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
        shiny::column(8,
                      shiny::selectInput(ns("variable"),
                                         "Select Variable",
                                         choices = c("mass", "density"),
                                         selected = "mass"
                                         )
                      )
      ),

      shiny::fluidRow(
        shiny::column(4, leaflet::leafletOutput(ns("albufera_map"))),
        shiny::column(8, plotly::plotlyOutput(ns("plot")))
      )
    ),

    shiny::tabPanel("Setup",
                    shiny::numericInput(ns("seed"),
                                        "Seed for simulation",
                                        value = 840,
                                        step = 1),
                    shiny::numericInput(ns("drift"),
                                        "drift",
                                        value = formals(compute_ct)$drift,
                                        step = 0.01,
                                        min = 0,
                                        max = 1),
                    shiny::numericInput(ns("covmax"),
                                        "covmax",
                                        value = formals(compute_ct)$covmax,
                                        step = 0.01,
                                        min = 0,
                                        max = 1),
                    shiny::numericInput(ns("jgrow"),
                                        "jgrow",
                                        value = formals(compute_ct)$jgrow,
                                        step = 1,
                                        min = 0,
                                        max = 400),
                    shiny::numericInput(ns("SNK"),
                                        "SNK",
                                        value = formals(compute_ct)$SNK,
                                        step = 1,
                                        min = 0,
                                        max = 400),
                    shiny::numericInput(ns("dact_m"),
                                        "dact_m",
                                        value = formals(compute_ct)$dact_m,
                                        step = 0.01,
                                        min = 0,
                                        max = 1),
                    shiny::numericInput(ns("css_ppm"),
                                        "css_ppm",
                                        value = formals(compute_ct)$css_ppm,
                                        step = 1,
                                        min = 0,
                                        max = 500),
                    shiny::numericInput(ns("bd_g_cm3"),
                                        "bd_g_cm3",
                                        value = formals(compute_ct)$bd_g_cm3,
                                        step = 0.01,
                                        min = 0,
                                        max = 10),
                    shiny::numericInput(ns("qseep_m_day"),
                                        "qseep_m_day",
                                        value = formals(compute_ct)$qseep_m_day,
                                        step = 0.01,
                                        min = 0,
                                        max = 10),
                    shiny::numericInput(ns("wilting"),
                                        "wilting",
                                        value = formals(compute_ct)$wilting,
                                        step = 0.01,
                                        min = 0,
                                        max = 1),
                    shiny::numericInput(ns("fc"),
                                        "fc",
                                        value = formals(compute_ct)$fc,
                                        step = 0.01,
                                        min = 0,
                                        max = 1)
    )


  )


}

ctServer <- function(id, simulation) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    res <- shiny::reactive({
      compute_ct(simulation = simulation(),
                 drift = input$drift,
                 covmax = input$covmax,
                 jgrow = input$jgrow,
                 SNK = input$SNK,
                 dact_m = input$dact_m,
                 css_ppm = input$css_ppm,
                 bd_g_cm3 = input$bd_g_cm3,
                 qseep_m_day = input$qseep_m_day,
                 wilting = input$wilting,
                 fc = input$fc
                 )
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
      plot(ct(res()), cluster_id = input$cluster_id, variable = input$variable)
    })

    return(res)
  })
}
