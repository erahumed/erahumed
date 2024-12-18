ct_input_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::numericInput(ns("drift"),
                        "drift",
                        value = formals(setup_ct)$drift,
                        step = 0.01,
                        min = 0,
                        max = 1),
    shiny::numericInput(ns("covmax"),
                        "covmax",
                        value = formals(setup_ct)$covmax,
                        step = 0.01,
                        min = 0,
                        max = 1),
    shiny::numericInput(ns("jgrow"),
                        "jgrow",
                        value = formals(setup_ct)$jgrow,
                        step = 1,
                        min = 0,
                        max = 400),
    shiny::numericInput(ns("SNK"),
                        "SNK",
                        value = formals(setup_ct)$SNK,
                        step = 1,
                        min = 0,
                        max = 400),
    shiny::numericInput(ns("dact_m"),
                        "dact_m",
                        value = formals(setup_ct)$dact_m,
                        step = 0.01,
                        min = 0,
                        max = 1),
    shiny::numericInput(ns("css_ppm"),
                        "css_ppm",
                        value = formals(setup_ct)$css_ppm,
                        step = 1,
                        min = 0,
                        max = 500),
    shiny::numericInput(ns("foc"),
                        "foc",
                        value = formals(setup_ct)$foc,
                        step = 0.001,
                        min = 0,
                        max = 1),
    shiny::numericInput(ns("bd_g_cm3"),
                        "bd_g_cm3",
                        value = formals(setup_ct)$bd_g_cm3,
                        step = 0.01,
                        min = 0,
                        max = 10),
    shiny::numericInput(ns("qseep_m_day"),
                        "qseep_m_day",
                        value = formals(setup_ct)$qseep_m_day,
                        step = 0.01,
                        min = 0,
                        max = 10),
    shiny::numericInput(ns("wilting"),
                        "wilting",
                        value = formals(setup_ct)$wilting,
                        step = 0.01,
                        min = 0,
                        max = 1),
    shiny::numericInput(ns("fc"),
                        "fc",
                        value = formals(setup_ct)$fc,
                        step = 0.01,
                        min = 0,
                        max = 1)
  )
}

ct_input_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::reactive({
      list(drift = input$drift,
           covmax = input$covmax,
           jgrow = input$jgrow,
           SNK = input$SNK,
           dact_m = input$dact_m,
           css_ppm = input$css_ppm,
           foc = input$foc,
           bd_g_cm3 = input$bd_g_cm3,
           qseep_m_day = input$qseep_m_day,
           wilting = input$wilting,
           fc = input$fc)
      })
  })
}

ctUI <- function(id) {
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
                      shiny::selectInput(ns("variable"),
                                         "Select Variable",
                                         choices = c("mass", "density"),
                                         selected = "mass"
                                         )
                      ),
        shiny::column(4,
                      shiny::downloadButton(ns("downloadData"), "Download Data")
                      )

      ),

      dygraphs::dygraphOutput(ns("plot")) |> shinycssloaders::withSpinner()

    ),

    shiny::tabPanel("Setup",
                    shiny::numericInput(ns("drift"),
                                        "drift",
                                        value = formals(setup_ct)$drift,
                                        step = 0.01,
                                        min = 0,
                                        max = 1),
                    shiny::numericInput(ns("covmax"),
                                        "covmax",
                                        value = formals(setup_ct)$covmax,
                                        step = 0.01,
                                        min = 0,
                                        max = 1),
                    shiny::numericInput(ns("jgrow"),
                                        "jgrow",
                                        value = formals(setup_ct)$jgrow,
                                        step = 1,
                                        min = 0,
                                        max = 400),
                    shiny::numericInput(ns("SNK"),
                                        "SNK",
                                        value = formals(setup_ct)$SNK,
                                        step = 1,
                                        min = 0,
                                        max = 400),
                    shiny::numericInput(ns("dact_m"),
                                        "dact_m",
                                        value = formals(setup_ct)$dact_m,
                                        step = 0.01,
                                        min = 0,
                                        max = 1),
                    shiny::numericInput(ns("css_ppm"),
                                        "css_ppm",
                                        value = formals(setup_ct)$css_ppm,
                                        step = 1,
                                        min = 0,
                                        max = 500),
                    shiny::numericInput(ns("foc"),
                                        "foc",
                                        value = formals(setup_ct)$foc,
                                        step = 0.001,
                                        min = 0,
                                        max = 1),
                    shiny::numericInput(ns("bd_g_cm3"),
                                        "bd_g_cm3",
                                        value = formals(setup_ct)$bd_g_cm3,
                                        step = 0.01,
                                        min = 0,
                                        max = 10),
                    shiny::numericInput(ns("qseep_m_day"),
                                        "qseep_m_day",
                                        value = formals(setup_ct)$qseep_m_day,
                                        step = 0.01,
                                        min = 0,
                                        max = 10),
                    shiny::numericInput(ns("wilting"),
                                        "wilting",
                                        value = formals(setup_ct)$wilting,
                                        step = 0.01,
                                        min = 0,
                                        max = 1),
                    shiny::numericInput(ns("fc"),
                                        "fc",
                                        value = formals(setup_ct)$fc,
                                        step = 0.01,
                                        min = 0,
                                        max = 1)
    )


  )


}

ctServer <- function(id, inp, hba, hbp, ca, shared) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    res <- shiny::reactive({
      simulation_from_layers(inp = inp(), hba = hba(), hbp = hbp(),ca = ca()) |>
        setup_ct(drift = input$drift,
                 covmax = input$covmax,
                 jgrow = input$jgrow,
                 SNK = input$SNK,
                 dact_m = input$dact_m,
                 css_ppm = input$css_ppm,
                 foc = input$foc,
                 bd_g_cm3 = input$bd_g_cm3,
                 qseep_m_day = input$qseep_m_day,
                 wilting = input$wilting,
                 fc = input$fc
                 ) |>
        run_simulation(layer = "ct") |>
        get_layer("ct")
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

      plot(res(), cluster_id = input$cluster_id, variable = input$variable)
    })

    output$downloadData <- shiny::downloadHandler(
      filename = function() paste0("output-ct-", Sys.Date(), ".csv"),
      content = \(file) readr::write_csv(get_layer_output(res()), file)
    )

    return(res)
  })
}
