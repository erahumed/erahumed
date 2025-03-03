ctc_input_ui <- function(id) {
  ns <- shiny::NS(id)

  tltp <- function(param) param_tooltip(layer = "ctc", param = param)


  shiny::tagList(
    shiny::numericInput(ns("drift"),
                        shiny::p("drift", tltp("drift")),
                        value = eval(formals(setup_exposure)$drift),
                        step = 0.01,
                        min = 0,
                        max = 1),
    shiny::numericInput(ns("covmax"),
                        shiny::p("covmax", tltp("covmax")),
                        value = eval(formals(setup_exposure)$covmax),
                        step = 0.01,
                        min = 0,
                        max = 1),
    shiny::numericInput(ns("jgrow"),
                        shiny::p("jgrow", tltp("jgrow")),
                        value = eval(formals(setup_exposure)$jgrow),
                        step = 1,
                        min = 0,
                        max = 400),
    shiny::numericInput(ns("SNK"),
                        shiny::p("SNK", tltp("SNK")),
                        value = eval(formals(setup_exposure)$SNK),
                        step = 1,
                        min = 0,
                        max = 400),
    shiny::numericInput(ns("dact_m"),
                        shiny::p("dact_m", tltp("dact_m")),
                        value = eval(formals(setup_exposure)$dact_m),
                        step = 0.01,
                        min = 0,
                        max = 1),
    shiny::numericInput(ns("css_ppm"),
                        shiny::p("css_ppm", tltp("css_ppm")),
                        value = eval(formals(setup_exposure)$css_ppm),
                        step = 1,
                        min = 0,
                        max = 500),
    shiny::numericInput(ns("foc"),
                        shiny::p("foc", tltp("foc")),
                        value = eval(formals(setup_exposure)$foc),
                        step = 0.001,
                        min = 0,
                        max = 1),
    shiny::numericInput(ns("bd_g_cm3"),
                        shiny::p("bd_g_cm3", tltp("bd_g_cm3")),
                        value = eval(formals(setup_exposure)$bd_g_cm3),
                        step = 0.01,
                        min = 0,
                        max = 10),
    shiny::numericInput(ns("qseep_m_day"),
                        shiny::p("qseep_m_day", tltp("qseep_m_day")),
                        value = eval(formals(setup_exposure)$qseep_m_day),
                        step = 0.01,
                        min = 0,
                        max = 10),
    shiny::numericInput(ns("wilting"),
                        shiny::p("wilting", tltp("wilting")),
                        value = eval(formals(setup_exposure)$wilting),
                        step = 0.01,
                        min = 0,
                        max = 1),
    shiny::numericInput(ns("fc"),
                        shiny::p("fc", tltp("fc")),
                        value = eval(formals(setup_exposure)$fc),
                        step = 0.01,
                        min = 0,
                        max = 1)
  )
}

ctc_input_server <- function(id) {
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

